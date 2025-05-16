CREATE TYPE codd_schema.obj_to_drop AS (
    kind text,
    objname text,
    on_ text
);
CREATE TABLE IF NOT EXISTS codd_schema.background_jobs (jobname TEXT NOT NULL PRIMARY KEY, status TEXT NOT NULL DEFAULT 'started', last_error TEXT, num_jobs_succeeded INT NOT NULL DEFAULT 0, num_jobs_error INT NOT NULL DEFAULT 0, job_func TEXT NOT NULL, objects_to_drop_in_order codd_schema.obj_to_drop[] NOT NULL, CHECK (status IN ('started', 'aborted', 'succeeded')));
CREATE FUNCTION codd_schema.unschedule_job_from_pg_cron() RETURNS TRIGGER AS $$
DECLARE
  func_name text;
BEGIN
  IF OLD.status='succeeded' AND NEW.status <> 'succeeded' THEN
    RAISE EXCEPTION 'Cannot change a background job status from succeeded to anything else';
  END IF;
  IF EXISTS (
     SELECT FROM cron.job WHERE jobname = OLD.jobname) THEN

     PERFORM cron.unschedule(OLD.jobname);
  END IF;
  IF TG_OP='DELETE' THEN
    RETURN OLD;
  END IF;
    
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
-- | This function is to be used live/in Production if the background job is interfering with your
-- application and you want to stop it.
-- It will not do any DDL and will just make the scheduled job stop running.
-- You should still run the synchronously_finish_* functions on this job later for DDL cleanup, but
-- you cannot resume this job. Rather, finish this and create a new one.
CREATE FUNCTION codd_schema.abort_background_job(job_name text) RETURNS VOID AS $$
DECLARE
  jobstatus text;
BEGIN
  -- TODO: Test aborting an incomplete job
  SELECT status INTO jobstatus FROM codd_schema.background_jobs WHERE jobname=job_name;
  IF jobstatus IS NULL THEN
    RAISE EXCEPTION 'Codd background job named % does not exist', job_name;
  ELSIF jobstatus='succeeded' THEN
    RAISE EXCEPTION 'Codd background job named % already succeeded, so it is not possible to abort it', job_name;
  END IF;
  UPDATE codd_schema.background_jobs SET status='aborted' WHERE background_jobs.jobname=job_name;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER stop_running_cron_job_on_abort_or_finish_update
    BEFORE UPDATE OF status
    ON codd_schema.background_jobs
    FOR EACH ROW
    WHEN (OLD.status NOT IN ('aborted', 'succeeded') AND NEW.status IN ('aborted', 'succeeded') AND OLD.status <> NEW.status)
    EXECUTE FUNCTION codd_schema.unschedule_job_from_pg_cron();
CREATE TRIGGER stop_running_cron_job_on_abort_or_finish_delete
    BEFORE DELETE
    ON codd_schema.background_jobs
    FOR EACH ROW
    WHEN (OLD.status='started')
    EXECUTE FUNCTION codd_schema.unschedule_job_from_pg_cron();

CREATE OR REPLACE FUNCTION codd_schema.assert_job_can_be_created(job_name text, cron_schedule text, plpgsql_to_run_periodically text) RETURNS VOID AS $func$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_extension WHERE extname='pg_cron') THEN
  -- TODO: Are there settings that need to be enabled for pg_cron to launch jobs?
    RAISE EXCEPTION 'Codd background migrations require the pg_cron extension to work. Please check https://github.com/citusdata/pg_cron for installation instructions';
  END IF;
  IF job_name IS NULL THEN
    RAISE EXCEPTION 'Please supply a job name';
  END IF;
  IF cron_schedule IS NULL THEN
    RAISE EXCEPTION 'Please supply a cron schedule. It could be e.g. "5 seconds" to run every 5 seconds or "0 10 * * *" for every day at 10AM. Check https://github.com/citusdata/pg_cron for more examples and explanations';
  END IF;
  IF plpgsql_to_run_periodically IS NULL THEN
    RAISE EXCEPTION 'Please supply a body of valid SQL or plpgsql to run on the supplied schedule';
  END IF;
  IF EXISTS (SELECT FROM codd_schema.background_jobs WHERE jobname=job_name) THEN
    RAISE EXCEPTION 'Codd background job named % already exists. Please choose another name or clean up the codd_schema.background_jobs table by deleting successful jobs if that would help', job_name;
  END IF;
END;
$func$ LANGUAGE plpgsql;

-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_job_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text) RETURNS VOID AS $func$
DECLARE
  temp_bg_success_func_name text := format('%I.%I', current_schema, '_codd_job_' || jobname);
  temp_bg_wrapper_func_name text := format('%I.%I', current_schema, '_codd_job_wrapper_' || jobname);
BEGIN
  PERFORM codd_schema.assert_job_can_be_created(jobname, cron_schedule, plpgsql_to_run_periodically);

  INSERT INTO codd_schema.background_jobs (jobname, job_func, objects_to_drop_in_order) VALUES (jobname, temp_bg_wrapper_func_name, ARRAY[ROW('FUNCTION', temp_bg_wrapper_func_name, NULL)::codd_schema.obj_to_drop, ROW('FUNCTION', temp_bg_success_func_name, NULL)::codd_schema.obj_to_drop]);

  -- Why two functions instead of just one? Because each function's effects are atomic, as if
  -- there was a savepoint around each function call, and that enables us to update codd_schema.background_jobs
  -- even if the job errors out. Still, write a test or look for official docs to assert this property.
  -- TODO: Test very long names for migrations (more than 63 chars).
  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS BIGINT AS $$
        DECLARE
          affected_row_count bigint;
        BEGIN
          %s
          GET DIAGNOSTICS affected_row_count = row_count;
          RETURN affected_row_count;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$, temp_bg_success_func_name, plpgsql_to_run_periodically);

  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS VOID AS $$
        DECLARE
          affected_row_count bigint;
        BEGIN
            SELECT %s() INTO affected_row_count;
            -- TODO: Can unscheduling cancel the job and rollback the changes applied in the last run? Check. https://github.com/citusdata/pg_cron/issues/308 suggests it might be possible.
            UPDATE codd_schema.background_jobs SET
                num_jobs_succeeded=num_jobs_succeeded+1
              , status=CASE WHEN affected_row_count = 0 THEN 'succeeded' ELSE status END
              WHERE jobname='%s';
        EXCEPTION WHEN OTHERS THEN
          -- The EXCEPTION clause silences the error from the logs (understandably) so it's important we
          -- emit at least a warning (we can't re-raise EXCEPTION or this function's UPDATE won't have an effect)
          RAISE WARNING 'Error in codd background job. %%s: %%s', SQLSTATE, SQLERRM;
          UPDATE codd_schema.background_jobs SET
              num_jobs_error=num_jobs_error + 1
            , last_error=format('%%s: %%s', SQLSTATE, SQLERRM)
            WHERE jobname='%s';
        END;
        $$ LANGUAGE plpgsql;
  $sqltext$, temp_bg_wrapper_func_name, temp_bg_success_func_name, jobname, jobname);

  -- TODO: Control isolation level
  PERFORM cron.schedule(jobname, cron_schedule, format($$BEGIN; SELECT %s(); COMMIT;$$, temp_bg_wrapper_func_name));
END;
$func$ LANGUAGE plpgsql;

-- | Synchronously runs a job until it completes (or does not run it if it's already complete) or until the supplied timeout elapses.
-- This does nothing if the job does not exist.
-- This doesn't run the job if it has been aborted.
-- This drops database objects created by the job if it completes successfully or if the job was aborted.
-- This does update codd's background_jobs table with the count of every successful and error run until either the timeout or successful completion. TODO: Add test that errors don't make the entire transaction fail.
-- This will run the job in the isolation level of the caller's (yours) in a single transaction regardless of how many times the job needs to run.
-- If the timeout elapses and the job isn't finished, this function will raise an exception, and will therefore fail to update even codd_schema.background_jobs properly.
-- This will drop the auxiliary functions created by codd if it completes successfully.
CREATE OR REPLACE FUNCTION codd_schema.synchronously_finish_background_job(job_name text, timeout interval) RETURNS VOID AS $func$
DECLARE
  start_time timestamptz := clock_timestamp();
  end_time timestamptz := clock_timestamp() + timeout;
  jobrow codd_schema.background_jobs;
  jobstatus text;
  obj_to_drop codd_schema.obj_to_drop;
BEGIN
  IF job_name IS NULL THEN
    RAISE EXCEPTION 'Please supply a job name';
  END IF;
  IF timeout IS NULL THEN
    RAISE EXCEPTION 'Please supply a timeout for synchronously completing a job';
  END IF;
  -- TODO: Can we pause the job while we run? Document if this is a limitation. Maybe LOCK FOR UPDATE/SKIP LOCKED in the functions just to avoid repeated work anyway?
  -- TODO: Add tests with aborted jobs

  SELECT * INTO jobrow FROM codd_schema.background_jobs WHERE background_jobs.jobname=job_name;
  IF jobrow.jobname IS NULL THEN
    RETURN;
  END IF;
  jobstatus := jobrow.status;
  WHILE jobstatus NOT IN ('aborted', 'succeeded') LOOP
    EXECUTE format('SELECT %s()', jobrow.job_func);
    -- TODO: Make function above return new status to avoid this extra query?
    SELECT status INTO jobstatus FROM codd_schema.background_jobs WHERE background_jobs.jobname=job_name;
    IF clock_timestamp() >= end_time AND jobstatus <> 'succeeded' THEN
      RAISE EXCEPTION 'Codd was unable to synchronously finish the background job %s in the supplied time limit. The job has not been aborted and will continue to run.', job_name;
    END IF;
  END LOOP;
  FOREACH obj_to_drop IN ARRAY jobrow.objects_to_drop_in_order
  LOOP
    EXECUTE format('DROP %s IF EXISTS %s %s', (obj_to_drop).kind, (obj_to_drop).objname, CASE WHEN (obj_to_drop).on_ IS NULL THEN '' ELSE format('ON %s', (obj_to_drop).on_) END);
  END LOOP;
END;
$func$ LANGUAGE plpgsql;

-- | Adds triggers and a background job to populate a column with values such that when the background job is completed,
-- every row in the table has been populated and new rows will also be automatically populated.
-- You can conclude the gradual migration with `synchronously_finish_background_job`, which will drop the triggers and
-- functions created here, but only after ensuring all rows are populated.
CREATE OR REPLACE FUNCTION codd_schema.populate_column_gradually(job_name text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, colname text, new_col_trigger_expr text) RETURNS VOID AS $func$
DECLARE
  trigger_name text;
  triggfn_name text;
  qualif_table_name text;
BEGIN
  PERFORM codd_schema.assert_job_can_be_created(job_name, cron_schedule, plpgsql_to_run_periodically);
  IF tablename IS NULL OR colname IS NULL OR new_col_trigger_expr IS NULL THEN
    RAISE EXCEPTION $err$
Did you forget to supply some arguments to populate_column_gradually? Here's an usage example that updates one row each second:

      ALTER TABLE animals ADD COLUMN new_number INT;
      SELECT codd_schema.populate_column_gradually('new-column-with-old-column-plus1', '1 seconds',
        $$
        UPDATE animals SET new_number=old_number+1 WHERE animal_id=(SELECT animal_id FROM animals WHERE new_number IS NULL LIMIT 1);
        $$
      , 'animals', 'new_number', 'NEW.old_number+1'
      );
    $err$;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_catalog.pg_attribute
                        JOIN pg_catalog.pg_class ON attrelid=pg_class.oid
                        JOIN pg_catalog.pg_namespace ON pg_class.relnamespace=pg_namespace.oid
                        WHERE attname=colname AND relname=tablename AND nspname=current_schema) THEN
      RAISE EXCEPTION 'Column % of relation % does not exist in your preferred schema. Please create the column yourself before calling populate_column_gradually, and possibly check your search_paths setting', colname, tablename; 
  END IF;
  trigger_name := format('%I', '_codd_bgjob_trig' || job_name);
  triggfn_name := format('%I.%I', current_schema, '_codd_bgjob_' || job_name);
  qualif_table_name := format('%I.%I', current_schema, tablename);
  
  -- TODO: Scheduling before creating the triggers might make the schedule task run before the triggers are created and return 0? Seems unlikely but pg_cron does execute things in separate connections. Oh, in that case the column might not even exist!
  --       So yeah, schedule with pg_cron AFTER creating the triggers! 
  -- TODO: Do BEFORE triggers override values explicitly defined in the VALUES list? We probably want them to?
  -- TODO: Should we forbid changing values of the new column with a trigger?
  -- TODO: Add tests with various search_paths to check we're being diligent
  -- TODO: Add tests with various weird object names
  PERFORM codd_schema.background_job_begin(job_name, cron_schedule, plpgsql_to_run_periodically);
  UPDATE codd_schema.background_jobs SET objects_to_drop_in_order=ARRAY[ROW('TRIGGER', trigger_name, qualif_table_name)::codd_schema.obj_to_drop, ROW('FUNCTION', triggfn_name, NULL)::codd_schema.obj_to_drop] || objects_to_drop_in_order WHERE jobname=job_name;
  EXECUTE format($triggers$
  CREATE FUNCTION %s() RETURNS TRIGGER AS $$
  BEGIN
    NEW.%I = %s;
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  CREATE TRIGGER %s
      BEFORE UPDATE OR INSERT
      ON %s
      FOR EACH ROW
      EXECUTE FUNCTION %s();
  $triggers$, triggfn_name, colname, new_col_trigger_expr, trigger_name, tablename, triggfn_name);
END;
$func$ LANGUAGE plpgsql;


