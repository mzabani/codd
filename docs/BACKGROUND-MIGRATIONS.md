# Background migrations

<!--toc:start-->
- [Background migrations](#background-migrations)
  - [Learning from example: migrating an INT column to BIGINT gradually](#learning-from-example-migrating-an-int-column-to-bigint-gradually)
<!--toc:end-->

When it is not feasible to `UPDATE/INSERT/DELETE` large numbers of rows in a migration because it would cause downtime when deployed, codd provides helper functions that allow those operations to run in the background, on a schedule.

With the [pg_cron](https://github.com/citusdata/pg_cron) extension installed (this is optional and you can implement your own job runner if you prefer, more on that near the end of this guide), here is an example of migrating an INTEGER column to a BIGINT one with codd:

## Learning from example: migrating an INT column to BIGINT gradually

Here's what your first migration could look like:
```sql
-- You must call 'codd.setup_background_worker' only once for your entire database,
-- and you must choose between 'pg_cron' and 'external'. 'pg_cron' requires the
-- extension but "just works", while 'external' means you will have to implement
-- your own job runner. We recommend 'pg_cron' unless you know better.
SELECT codd.setup_background_worker('pg_cron');

-- Now we add the column that will gradually populate the new 'new_employee_id'
-- column, and for the sake of an example, we update 1000 rows every 10 seconds.
-- The 'codd.populate_column_gradually' function will create a background job
-- and triggers to keep the values in 'employee.new_employee_id' in sync.
-- The job will automatically stop running when the `UPDATE` statement has no
-- more rows to update.
ALTER TABLE employee ADD COLUMN new_employee_id BIGINT;
SELECT codd.populate_column_gradually('make-employee_id-a-bigint', '10 seconds',
-- Next is the job that will run every 10 seconds. Note that row-returning statements do not work here.
$$
UPDATE employee SET new_employee_id=employee_id::bigint
WHERE employee_id IN (SELECT employee_id FROM employee WHERE new_employee_id IS NULL LIMIT 1000);
$$
, 'employee', 'new_employee_id',
-- Next is the trigger expression which will be used for INSERTs and UPDATEs
'NEW.employee_id::bigint'
);
```

When you add the migration above you should see some instructions, too, but let's run `SELECT * FROM codd.jobs` to see what happens:
```psql

jobname          |          created_at           |  status   |                                                   description                                                   | num_jobs_succeeded | num_jobs_error |          last_run_at          |    completed_or_aborted_at    |         finalized_at          |         last_error_at         |                            last_error
---------------------------+-------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------+--------------------+----------------+-------------------------------+-------------------------------+-------------------------------+-------------------------------+-------------------------------------------------------------------
 make-employee_id-a-bigint | 2025-07-26 17:19:19.641087+00 | started   | Gradually populating values in the employee.new_employee_id column                                              |                  0 |              0 |                               |                               |                               |                               |
```

You will be able to run this in any environments you deploy to, and after waiting long enough every row in the `employee` table will have its `new_employee_id` column synced to `employee_id`. Even newly inserted or updated rows will be that way due to the added triggers.

After a while, this is what you will see in `codd.jobs`:

```sql
> SELECT jobname, status, description FROM codd.jobs;
          jobname          |               status               |                                                                                                                             description
          ---------------------------+------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
           make-employee_id-a-bigint | run-complete-awaiting-finalization | Every row in table employee now has the new_employee_id column populated and background jobs are no longer running. You can now call codd.synchronously_finalize_background_job to remove the triggers and accessory functions created to keep the new column up-to-date
```

The status will be `run-complete-awaiting-finalization`, and the description will lay it out more clearly what that means. Supposing every environment is showing us this same status, let's follow that description and create our second migration:


```sql
-- This will remove triggers created to keep the two columns in sync, and will apply the
-- job's SQL repeatedly until it updates 0 rows (which will not happen in practice in environments
-- where the status is 'run-complete-awaiting-finalization' since every row has already been updated).
-- The second argument is a time limit to finalize all of the above. You can safely use '0 seconds'
-- if the status is 'run-complete-awaiting-finalization' in all environments this will be
-- applied in, but local development environments may e.g. not have pg_cron running, meaning
-- this function will have to update all rows and might require a bit longer, hence why we
-- use '10 seconds' below. You can, of course, apply this with different time limits in different
-- environments, if you prefer.
SELECT codd.synchronously_finalize_background_job('make-employee_id-a-bigint', '10 seconds');

-- You still have to rename and drop columns yourself. The 'codd.populate_column_gradually' function
-- does only that: populate the new column.
ALTER TABLE employee ALTER COLUMN new_employee_id SET DEFAULT nextval('employee_employee_id_seq');
ALTER SEQUENCE employee_employee_id_seq OWNED BY employee.new_employee_id;
ALTER TABLE employee DROP COLUMN employee_id;
ALTER TABLE employee RENAME COLUMN new_employee_id TO employee_id;
```

## Ignoring the 'cron' namespace

TODO

## What if I can't use pg_cron?

Then you will have to call `SELECT codd.setup_background_worker('external')` to set things up, and unless you implement a job runner (there is no documentation how to do that yet) there will be no background jobs running at all. You can still call all the background-job functions codd provides, but without a job runner this will only be useful in development, testing or environments with small databases. **If you are in this position, please file a request for codd to implement its own job runner**; it's already in the author's plans, but he has no sense of when people might need it.

**If you are only in this position due to development and testing environments**, you probably don't need to worry. Our recommendation is that
There are likely two motives for this: pg_cron only working with one database per instance or
Sadly, pg_cron forces its users to choose a single database to work in.
In development environments where you might have a development database and a database to run tests with, a conundrum naturally arises: how can I have my codd migrations work in both of these environments?

The answer is that 
