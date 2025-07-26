-- codd: requires-codd-schema
-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive.
-- This will remove triggers created to keep the two columns in sync, and will apply the
-- job's SQL repeatedly until it updates 0 rows (which will not happen in practice in environments
-- where the status is 'run-complete-awaiting-finalization').
-- The second argument is a time limit to finalize all of the above. You can safely use '0 seconds'
-- if the status is 'run-complete-awaiting-finalization' in all environments this will be
-- applied in, but local development environments may e.g. not have pg_cron running, meaning
-- this function will have to update all rows and might require a bit longer, hence why we
-- use '10 seconds' below. You can, of course, apply this with different time limits in different
-- environments, if you prefer.
SELECT codd.synchronously_finalize_background_job('make-employee_id-a-bigint', '1 seconds');

-- You still have to rename and drop columns yourself. The 'codd.populate_column_gradually' function
-- does only that: populate the new column.
ALTER TABLE employee ALTER COLUMN new_employee_id SET DEFAULT nextval('employee_employee_id_seq');
ALTER SEQUENCE employee_employee_id_seq OWNED BY employee.new_employee_id;
ALTER TABLE employee DROP COLUMN employee_id;
ALTER TABLE employee RENAME COLUMN new_employee_id TO employee_id;

