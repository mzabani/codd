# Background migrations

<!--toc:start-->
- [Background migrations](#background-migrations)
  - [Learning from example: migrating an INT column to BIGINT gradually](#learning-from-example-migrating-an-int-column-to-bigint-gradually)
  - [Installing pg_cron](#installing-pgcron)
  - [Valid cron/periodicity expressions for background jobs](#valid-cronperiodicity-expressions-for-background-jobs)
  - [Aborting background migrations](#aborting-background-migrations)
  - [What if I can't use pg_cron?](#what-if-i-cant-use-pgcron)
  - [Ignoring the 'cron' namespace](#ignoring-the-cron-namespace)
  - [More complex background migrations](#more-complex-background-migrations)
<!--toc:end-->

When it is not feasible to `UPDATE/INSERT/DELETE` large numbers of rows in a migration because it would cause downtime when deployed, codd provides helper functions that allow those operations to run in the background, on a schedule.

With the [pg_cron](https://github.com/citusdata/pg_cron) extension installed (this is optional and you can implement your own job runner if you prefer, more on that near the end of this guide).

This is a recent feature of codd, so please feel free to contribute with ideas through GitHub's issue tracker!

## Learning from example: migrating an INT column to BIGINT gradually

Here's what your first migration could look like (read [Installing pg_cron](#installing-pgcron) first if you wish to try this!):
```sql
-- You must call 'codd.setup_background_worker' only once for your entire database,
-- and you must choose between 'pg_cron' and 'external'. 'pg_cron' requires the
-- extension but "just works", while 'external' means you will have to implement
-- your own job runner. We recommend 'pg_cron' unless you know better.
SELECT codd.setup_background_worker('pg_cron');

-- Now we add the column that will gradually populate the new 'new_employee_id'
-- column, and for the sake of an example, we update 1000 rows every 10 seconds.
-- The 'codd.populate_table_gradually' function will create a background job
-- and triggers to keep the values in 'employee.new_employee_id' in sync.
-- The job will automatically stop running when the `UPDATE` statement has no
-- more rows to update.
ALTER TABLE employee ADD COLUMN new_employee_id BIGINT;
SELECT codd.populate_table_gradually('make-employee_id-a-bigint', '10 seconds', 'employee',
-- Next is the job that will run every 10 seconds. Note that row-returning statements do not work here.
$$
UPDATE employee SET new_employee_id=employee_id::bigint
WHERE employee_id IN (SELECT employee_id FROM employee WHERE new_employee_id IS NULL LIMIT 1000);
$$,
-- Next is the trigger expression which will be used for INSERTs and UPDATEs
'NEW.new_employee_id=NEW.employee_id::bigint'
);
```

When you add the migration above you should see some instructions, too, but let's run `SELECT * FROM codd.jobs` to see what happens:
```psql

jobname                    |          created_at           |  status   |                                                   description                                                   | num_jobs_succeeded | num_jobs_error |          last_run_at          |    completed_or_aborted_at    |         finalized_at          |         last_error_at         |                            last_error
---------------------------+-------------------------------+-----------+-----------------------------------------------------------------------------------------------------------------+--------------------+----------------+-------------------------------+-------------------------------+-------------------------------+-------------------------------+-------------------------------------------------------------------
 make-employee_id-a-bigint | 2025-07-26 17:19:19.641087+00 | started   | Gradually populating values in the employee table                                          |                  0 |                  0 |                |                               |                               |                               |
```

You will be able to run this in any environments you deploy to, and after waiting long enough every row in the `employee` table will have its `new_employee_id` column synced to `employee_id`. Even newly inserted or updated rows will be that way due to the added triggers.

After a while, this is what you will see in `codd.jobs`:

```sql
> SELECT jobname, status, description FROM codd.jobs;
jobname                   |               status               |                                                                                                                             description
--------------------------+------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
make-employee_id-a-bigint | run-complete-awaiting-finalization | Every row in table employee has now been populated and background jobs are no longer running. You can now call codd.synchronously_finalize_background_job to remove the triggers and accessory functions created to keep the rows up-to-date
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

-- You still have to rename and drop columns yourself. The 'codd.populate_table_gradually' function
-- does only that: populate the new column.
ALTER TABLE employee ALTER COLUMN new_employee_id SET DEFAULT nextval('employee_employee_id_seq');
ALTER SEQUENCE employee_employee_id_seq OWNED BY employee.new_employee_id;
ALTER TABLE employee DROP COLUMN employee_id;
ALTER TABLE employee RENAME COLUMN new_employee_id TO employee_id;
```

## Installing pg_cron

After/if you install the pg_cron extension (this can be done as per your cloud provider's docs or other installation methods depending on how you manage your postgres instances), you also must grant usage to the user that runs your codd migrations, the one in your `CODD_CONNECTION` environment variable, e.g.:

```sql
GRANT USAGE ON SCHEMA cron TO your_user_here;
```

## Valid cron/periodicity expressions for background jobs

In the examples above we use `'10 seconds'`, but any pg_cron expression is valid. As per the [official pg_cron docs](https://github.com/citusdata/pg_cron) this is either a `'[1-59] seconds'` string, or a string in the format:

```
 ┌───────────── min (0 - 59)
 │ ┌────────────── hour (0 - 23)
 │ │ ┌─────────────── day of month (1 - 31) or last day of the month ($)
 │ │ │ ┌──────────────── month (1 - 12)
 │ │ │ │ ┌───────────────── day of week (0 - 6) (0 to 6 are Sunday to
 │ │ │ │ │                  Saturday, or use names; 7 is also Sunday)
 │ │ │ │ │
 │ │ │ │ │
 * * * * *
```

Examples include:
- Saturday at 3:30am (GMT) is `'30 3 * * 6'`
- Every day at 10:00am (GMT) is `'0 10 * * *'`

## Aborting background migrations

You **can** abort codd's background jobs in live environments and that will not cause any schema modifications (i.e. no DDL will be applied), so this is designed to keep your applications working. The only consequence of aborting a background job is the scheduled jobs no longer run, so use this if the background job is interfering negatively with your application. Here's how to do it:

````sql
SELECT codd.abort_background_job('make-employee_id-a-bigint');
````

You can also query `codd.jobs` after this, and its description should now give you instructions, such as:

```
Given up populating values in the employee.new_employee_id column. You can DELETE this job row from codd._background_jobs without any side-effects and do any DDL you deem necessary now
````

Make sure you don't `codd.synchronously_finalize_background_job` aborted migrations, as it will fail. It's probably easier if you abort the same job in every environment, delete the row from `codd._background_jobs` in every environment and try again with a new migration.

## What if I can't use pg_cron?

Then you will have to call `SELECT codd.setup_background_worker('external')` to set things up, and unless you implement a job runner (there is no documentation how to do that yet) there will be no background jobs running at all. You can still call all the background-job functions codd provides, but without a job runner this will only be useful in development, testing or environments with small databases. **If you are in this position, please file a request for codd to implement its own job runner**; it's already in the author's plans, but he has no sense of when people might need it.

**If you are only in this position due to development and testing environments**, you probably don't need to worry.
Our recommendation is that you install and set up the `pg_cron` runner in environments where databases are large, like Production and other similar environments, and the `external` runner in local development and testing environments.

You can do that by having distinct folders per environment for your migrations (like we do in [START-USING.md] with a `dev-only` folder), and calling `codd.setup_background_worker` differently in each migration according to their environment.

## Ignoring the 'cron' namespace

If you install pg_cron in a database, it creates a `cron` namespace. Codd will pick up that namespace for schema equality checks, and that can be undesirable unless you have pg_cron in every environment.

Until we implement [customizable schema equality checks](https://github.com/mzabani/codd/issues/167), the best way we have to ignore the `cron` schema entirely is to include every relevant namespace in the `CODD_SCHEMAS` environment variable (which means not including `cron` there). That environment variable is a space separated list of namespaces you want to check.

## More complex background migrations

You can implement your own by using the more primitive `codd.background_job_begin` function. The author does not promise API or functional stability for it at the moment, but you can run `\ef codd.populate_table_gradually` to view how `populate_table_gradually` uses it.
