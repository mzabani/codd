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
-- | This function is to be used as an emergency. It will not do any DDL and will just make the scheduled job
-- stop running. You can still run the synchronously_finish_* functions on the job later for DDL cleanup if you wish.
CREATE FUNCTION codd_schema.abort_background_job(jobname text) RETURNS VOID AS $$
BEGIN
  UPDATE codd_schema.background_jobs SET status='aborted' WHERE background_jobs.jobname=jobname;
END;
$$ LANGUAGE plpgsql STRICT;
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

-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_job_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text, schema_for_objects text = 'codd_schema') RETURNS VOID AS $func$
DECLARE
  temp_bg_success_func_name text := format('%I.%I', schema_for_objects, '_codd_job_' || jobname);
  temp_bg_wrapper_func_name text := format('%I.%I', schema_for_objects, '_codd_job_wrapper_' || jobname);
BEGIN
  -- TODO: Check for NULLs and throw
  -- TODO: Call function that checks that pg_cron exists and is set up properly and raises error or info
  -- TODO: Check the user calling this function is the same that create codd_schema? Users can only view jobs
  --       in `cron.job` that were created by them due to a RLS policy.
  -- Note that cron.job_run_details might not be turned on.. so we can't rely on it!

  -- Insert into table first so there are no side effects if this is a duplicate name
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
  -- TODO: Throw if args are null
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
    EXECUTE format($$DROP %s IF EXISTS %s %s$$, (obj_to_drop).kind, (obj_to_drop).objname, CASE WHEN (obj_to_drop).on_ IS NULL THEN '' ELSE format('ON %s', (obj_to_drop).on_) END);
  END LOOP;
END;
$func$ LANGUAGE plpgsql;

-- | Adds triggers and a background job to populate a column with values such that when the background job is completed,
-- every row in the table has been populated and new rows will also be automatically populated.
-- You can conclude the gradual migration with `synchronously_finish_background_job`, which will drop the triggers and
-- functions created here, but only after ensuring all rows are populated.
CREATE OR REPLACE FUNCTION codd_schema.populate_column_gradually(job_name text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, colname text, new_col_trigger_expr text) RETURNS VOID AS $func$
DECLARE
  trigger_name text := format('%I', '_codd_bgjob_trig' || job_name);
  triggfn_name text := format('%I.%I', current_schema, '_codd_bgjob_' || job_name);
  qualif_table_name text := format('%I.%I', current_schema, tablename);
BEGIN
  -- TODO: Check for a bunch of things like the other function does
  -- TODO: Check old and new column exist, tell user the new column must be created by them before calling this function
  -- TODO: Scheduling before creating the triggers might make the schedule task run before the triggers are created and return 0? Seems unlikely but pg_cron does execute things in separate connections. Oh, in that case the column might not even exist!
  --       So yeah, schedule with pg_cron AFTER creating the triggers! 
  -- TODO: Do BEFORE triggers override values explicitly defined in the VALUES list? We probably want them to?
  -- TODO: Do we forbid changing values of the new column with a trigger?
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


