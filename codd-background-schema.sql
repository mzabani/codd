CREATE TYPE codd_schema.obj_to_drop AS (
    kind text,
    objname text,
    on_ text
);
CREATE TABLE IF NOT EXISTS codd_schema.background_jobs (jobname TEXT NOT NULL PRIMARY KEY, status TEXT NOT NULL DEFAULT 'started', last_error TEXT, num_jobs_no_error INT NOT NULL DEFAULT 0, num_jobs_error INT NOT NULL DEFAULT 0, runs_outside_txn BOOL NOT NULL, job_func_or_stmt TEXT NOT NULL, objects_to_drop_in_order codd_schema.obj_to_drop[] NOT NULL, pg_cron_jobs TEXT[] NOT NULL, CHECK (status IN ('started', 'aborted', 'succeeded')));
CREATE FUNCTION codd_schema.unschedule_job_from_pg_cron() RETURNS TRIGGER AS $$
DECLARE
  pg_cron_job_name text;
BEGIN
  IF OLD.status='succeeded' AND NEW.status <> 'succeeded' THEN
    RAISE EXCEPTION 'Cannot change a background job status from succeeded to anything else';
  END IF;
  FOREACH pg_cron_job_name IN ARRAY OLD.pg_cron_jobs
  LOOP
    IF EXISTS (SELECT FROM cron.job WHERE jobname = pg_cron_job_name) THEN
       PERFORM cron.unschedule(pg_cron_job_name);
    END IF;
  END LOOP;
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
  IF EXISTS (SELECT FROM cron.job WHERE jobname=job_name) THEN
    RAISE EXCEPTION 'There already exists a pg_cron job named %. Please choose another name or clean up the the list of pg_cron jobs', job_name;
  END IF;
END;
$func$ LANGUAGE plpgsql;

CREATE FUNCTION codd_schema._append_semi_colon(plpgsqltext text) RETURNS TEXT AS $$
BEGIN
    RETURN CASE WHEN plpgsqltext ~ ';\s*$' THEN plpgsqltext ELSE plpgsqltext || ';' END;
END;
$$ IMMUTABLE STRICT PARALLEL SAFE LANGUAGE plpgsql;
CREATE TYPE codd_schema.succeeded_signal_kind AS ENUM ('when-modifies-0-rows', 'select-boolean-into-succeeded');

-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_job_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text, succeeded_signal codd_schema.succeeded_signal_kind = 'when-modifies-0-rows') RETURNS VOID AS $func$
DECLARE
  temp_bg_success_func_name text := format('%I.%I', current_schema, '_codd_job_' || jobname);
  temp_bg_wrapper_func_name text := format('%I.%I', current_schema, '_codd_job_wrapper_' || jobname);
BEGIN
  PERFORM codd_schema.assert_job_can_be_created(jobname, cron_schedule, plpgsql_to_run_periodically);

  INSERT INTO codd_schema.background_jobs (jobname, job_func_or_stmt, runs_outside_txn, objects_to_drop_in_order, pg_cron_jobs) VALUES (jobname, temp_bg_wrapper_func_name, FALSE, ARRAY[ROW('FUNCTION', temp_bg_wrapper_func_name, NULL)::codd_schema.obj_to_drop, ROW('FUNCTION', temp_bg_success_func_name, NULL)::codd_schema.obj_to_drop], ARRAY[jobname]);

  -- Why two functions instead of just one? Because each function's effects are atomic, as if
  -- there was a savepoint around each function call, and that enables us to update codd_schema.background_jobs
  -- even if the job errors out. Still, write a test or look for official docs to assert this property.
  -- TODO: Test very long job names (more than 63 chars) and with weird characters too
  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS BOOLEAN AS $$
        DECLARE
          affected_row_count bigint;
          succeeded boolean;
        BEGIN
          %s
          %s
          RETURN succeeded;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$, temp_bg_success_func_name, codd_schema._append_semi_colon(plpgsql_to_run_periodically),
        CASE WHEN succeeded_signal='when-modifies-0-rows' THEN $$
          GET DIAGNOSTICS affected_row_count = row_count;
          succeeded := (affected_row_count=0);$$ ELSE '' END);

  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS VOID AS $$
        DECLARE
          succeeded boolean;
          stack text;
        BEGIN
            SELECT %s() INTO succeeded;
            -- TODO: Can unscheduling cancel the job and rollback the changes applied in the last run? Check. https://github.com/citusdata/pg_cron/issues/308 suggests it might be possible.
            UPDATE codd_schema.background_jobs SET
                num_jobs_no_error=num_jobs_no_error+1
              , status=CASE WHEN succeeded THEN 'succeeded' ELSE status END
              WHERE jobname=%s;
        EXCEPTION WHEN OTHERS THEN
          -- The EXCEPTION clause silences the error from the logs (understandably) so it's important we
          -- emit at least a warning (we can't re-raise EXCEPTION or this function's UPDATE won't have an effect)
          GET DIAGNOSTICS stack = PG_CONTEXT;
          RAISE WARNING 'Error in codd background job. %% %%. Stack: %%', SQLSTATE, SQLERRM, stack;
          UPDATE codd_schema.background_jobs SET
              num_jobs_error=num_jobs_error + 1
            , last_error=format('%%s: %%s', SQLSTATE, SQLERRM)
            WHERE jobname=%s;
        END;
        $$ LANGUAGE plpgsql;
  $sqltext$, temp_bg_wrapper_func_name, temp_bg_success_func_name, quote_literal(jobname), quote_literal(jobname));

  -- TODO: Control isolation level
  PERFORM cron.schedule(jobname, cron_schedule, format('BEGIN; SELECT %s(); COMMIT;', temp_bg_wrapper_func_name));
END;
$func$ LANGUAGE plpgsql;

-- | Synchronously runs a job until it completes (or does not run it if it's already complete) or until the supplied timeout elapses.
-- This does nothing if the job does not exist.
-- This doesn't run the job if it has been aborted.
-- This drops database objects created by the job if it completes successfully or if the job was aborted.
-- This does update codd's background_jobs table with the count of every successful and error run until either the timeout or successful completion. TODO: Add test that errors don't make the entire transaction fail.
-- This will run the job in the isolation level of the caller's (yours) in a single transaction regardless of how many times the job needs to run.
-- If the timeout elapses and the job isn't succeeded, this function will raise an exception, and will therefore fail to update even codd_schema.background_jobs properly.
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
  -- TODO: Can we pause the job while we run? Document if this is a limitation. Maybe LOCK FOR UPDATE/SKIP LOCKED in the functions just to avoid repeated work anyway? Our tests should run with serializable isolation as that's the hardest level for this kind of thing
  -- TODO: Add tests with aborted jobs

  SELECT * INTO jobrow FROM codd_schema.background_jobs WHERE background_jobs.jobname=job_name;
  IF jobrow.runs_outside_txn IS TRUE THEN
    RAISE EXCEPTION 'It is not possible to synchronously force a job that runs outside transactions to finish';
  END IF;
  IF jobrow.jobname IS NULL THEN
    RETURN;
  END IF;
  jobstatus := jobrow.status;
  WHILE jobstatus NOT IN ('aborted', 'succeeded') LOOP
    EXECUTE format('SELECT %s()', jobrow.job_func_or_stmt);
    -- TODO: Make function above return new status to avoid this extra query?
    SELECT status INTO jobstatus FROM codd_schema.background_jobs WHERE background_jobs.jobname=job_name;
    IF clock_timestamp() >= end_time AND jobstatus <> 'succeeded' THEN
      RAISE EXCEPTION 'Codd was unable to synchronously finish the background job % in the supplied time limit. The job has not been aborted and will continue to run.', job_name;
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
Did you forget to supply some arguments to populate_column_gradually? Here's an usage example that updates one row every second:

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
  -- TODO: Should we forbid changing values of the new column with a trigger? Or does the BEFORE trigger make such attempts futile?
  -- TODO: Add tests with various search_paths to check we're being diligent
  -- TODO: Add tests with various weird object names
  PERFORM codd_schema.background_job_begin(job_name, cron_schedule, plpgsql_to_run_periodically);
  UPDATE codd_schema.background_jobs SET objects_to_drop_in_order=ARRAY[ROW('TRIGGER', trigger_name, qualif_table_name)::codd_schema.obj_to_drop, ROW('FUNCTION', triggfn_name, NULL)::codd_schema.obj_to_drop] || objects_to_drop_in_order WHERE jobname=job_name;
  EXECUTE format($triggers$
  CREATE FUNCTION %s() RETURNS TRIGGER AS $$
  BEGIN
    NEW.%I = %s
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  CREATE TRIGGER %s
      BEFORE UPDATE OR INSERT
      ON %s
      FOR EACH ROW
      EXECUTE FUNCTION %s();
  $triggers$, triggfn_name, colname, codd_schema._append_semi_colon(new_col_trigger_expr), trigger_name, tablename, triggfn_name);
END;
$func$ LANGUAGE plpgsql;

-- | I just want to check this is possible; we don't need it as much as e.g. populating columns and we'll need schema ignore rules before this works nicely with codd, too
CREATE OR REPLACE FUNCTION codd_schema.create_index_concurrently(job_name text, check_completion_cron_schedule text, create_index_concurrently_if_not_exists_statement text, tablename text, indexname text, try_create_cron_schedule text) RETURNS VOID AS $func$
DECLARE
  qualif_table_name text;
BEGIN
  -- TODO: Raise exception that disables this function completely until we have schema ignore rules in place to support it well
  -- TODO: Check that no pg_cron job with the "-try-create" ending exists
  -- TODO: If there is a schedule for trying (as opposed to just trying once), then we should be able to handle failure well, too.
  -- TODO: We need to make the DDL change run only once.. things like REINDEX CONCURRENTLY don't have an idempotent form available, and we don't want those to just keep on recreating indexes repeatedly. Maybe for now we ask users to `DROP INDEX` and `CREATE INDEX` again and just document this limitation.
  PERFORM codd_schema.background_job_begin(job_name, check_completion_cron_schedule,
    format($$
      SELECT (COUNT(*)=1) INTO succeeded
      FROM pg_catalog.pg_index
      JOIN pg_catalog.pg_class index_class ON indexrelid=index_class.oid
      JOIN pg_catalog.pg_namespace ON relnamespace=pg_namespace.oid
      JOIN pg_catalog.pg_class index_table ON index_table.oid=indrelid
      WHERE nspname=%s AND index_class.relname=%s AND index_table.relname=%s AND indisready AND indislive AND indisvalid;$$, quote_literal(current_schema), quote_literal(indexname), quote_literal(tablename))
    , 'select-boolean-into-succeeded');

  -- Add another cron job that does not run in a transaction to create the index
  UPDATE codd_schema.background_jobs
    SET pg_cron_jobs = pg_cron_jobs || ARRAY[job_name || '-try-create']
    WHERE jobname=job_name;
  PERFORM cron.schedule(job_name || '-try-create', try_create_cron_schedule, create_index_concurrently_if_not_exists_statement);
END;
$func$ LANGUAGE plpgsql;
