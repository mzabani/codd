-- codd: requires-codd-schema
-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive.
-- You must call setup_background_worker only once for your entire database,
-- and you must choose between 'pg_cron' and 'external'. 'pg_cron' requires the
-- extension but "just works", while 'external' means you will have to implement
-- your own job runner. We recommend 'pg_cron' unless you know better.
SELECT codd.setup_background_worker('pg_cron');

-- Now we add the column that will eventually replace the existing 'employee_id'
-- column, and for the sake of an example, we update 1000 rows every 10 seconds.
-- The 'codd.populate_table_gradually' function will create a background job
-- and triggers to keep the 'employee.new_employee_id' in sync.
-- The job will automatically stop running when the `UPDATE` statement has no
-- more rows to update, and will will be able to follow the progress through the
-- 'codd.jobs' view.
ALTER TABLE employee ADD COLUMN new_employee_id BIGINT;
SELECT codd.populate_table_gradually('make-employee_id-a-bigint', '10 seconds', 'employee',
$$
UPDATE employee SET new_employee_id=employee_id::bigint
WHERE employee_id IN (SELECT employee_id FROM employee WHERE new_employee_id IS NULL LIMIT 1000);
$$
, 'NEW.new_employee_id=NEW.employee_id::bigint'
);
