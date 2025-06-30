{-# LANGUAGE QuasiQuotes #-}

module Codd.InternalSchema.V5 (migrateInternalSchemaV4ToV5) where

import Codd.Query (InTxn, pgExecvoid_)
import Database.PostgreSQL.Query.TH.SqlExp (sqlExp)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO)

migrateInternalSchemaV4ToV5 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
migrateInternalSchemaV4ToV5 conn = do
  pgExecvoid_
    conn
    [sqlExp|
ALTER SCHEMA codd_schema RENAME TO codd;
CREATE TABLE codd._background_worker_type (
  id INT NOT NULL PRIMARY KEY CHECK (id = 1) -- Enforce a single row
  , worker_type TEXT NOT NULL CHECK (worker_type IN ('pg_cron', 'external'))
);
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE codd._background_worker_type TO PUBLIC;
CREATE FUNCTION codd.setup_background_worker(worker_type TEXT) RETURNS VOID AS $$
BEGIN
  NOTIFY "codd.___require_codd_schema_channel";
  IF worker_type NOT IN ('pg_cron', 'external') THEN
    RAISE EXCEPTION 'Background workers supported by codd are either "pg_cron", which requires the extension, or "external", which requires you to set up an external job runner yourself to periodically run scheduled jobs.';
  END IF;
  IF worker_type = 'pg_cron' AND NOT EXISTS (SELECT FROM pg_catalog.pg_extension WHERE extname='pg_cron') THEN
    -- TODO: Are there settings that need to be enabled for pg_cron to launch jobs?
    RAISE EXCEPTION 'Setting up codd background migrations with pg_cron requires the pg_cron extension to be installed first. Please check https://github.com/citusdata/pg_cron for installation instructions';
  END IF;

  INSERT INTO codd._background_worker_type (id, worker_type) VALUES (1, worker_type)
  ON CONFLICT (id) DO UPDATE SET worker_type=EXCLUDED.worker_type;
END;
$$ LANGUAGE plpgsql;
CREATE FUNCTION codd._assert_worker_is_setup() RETURNS VOID AS $$
BEGIN
  IF NOT EXISTS (SELECT FROM codd._background_worker_type) THEN
    RAISE EXCEPTION 'You must call the codd.setup_background_worker function before scheduling jobs. You can either add `SELECT codd.setup_background_worker(''pg_cron'')` or `SELECT codd.setup_background_worker(''external'')` to your migration depending on whether you would like the pg_cron extension to run periodic jobs or if you wish to implement a job runner yourself otherwise';
  END IF;
END;
$$ LANGUAGE plpgsql;
CREATE TYPE codd.obj_to_drop AS (
    kind text,
    objname text,
    on_ text
);
CREATE TABLE codd._background_jobs (
  jobid SERIAL PRIMARY KEY,
  jobname TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'started',
  created_at TIMESTAMPTZ NOT NULL DEFAULT CLOCK_TIMESTAMP(),
  created_by_role TEXT NOT NULL DEFAULT current_user,
  txn_isolation_level TEXT NOT NULL DEFAULT COALESCE(CURRENT_SETTING('codd.___txn_isolation_level', TRUE), 'db-default'),
  last_run_at TIMESTAMPTZ,
  last_error_at TIMESTAMPTZ,
  completed_or_aborted_at TIMESTAMPTZ,
  finalized_at TIMESTAMPTZ,
  num_jobs_succeeded BIGINT NOT NULL DEFAULT 0,
  num_jobs_error BIGINT NOT NULL DEFAULT 0,
  last_error TEXT,
  description_started TEXT,
  description_aborted TEXT NOT NULL DEFAULT 'You may delete this row from codd._background_jobs at any time with no side effects',
  description_awaiting_finalization TEXT,
  description_finalized TEXT NOT NULL DEFAULT 'Job completed successfully. You may delete this row from codd._background_jobs at any time with no side effects',
  job_function TEXT NOT NULL,
  objects_to_drop_in_order codd.obj_to_drop[] NOT NULL,
  pg_cron_jobs TEXT[] NOT NULL
  , UNIQUE (jobname)
  , CHECK (txn_isolation_level IN ('db-default', 'read-uncommitted', 'read-committed', 'repeatable-read', 'serializable'))
  , CHECK (status IN ('started', 'aborted', 'run-complete-awaiting-finalization', 'finalized'))
  , CHECK (completed_or_aborted_at IS NULL OR status <> 'started')
  , CHECK ((finalized_at IS NOT NULL) = (status = 'finalized'))
);
GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE codd._background_jobs TO PUBLIC;
CREATE VIEW codd.jobs AS
  SELECT jobname, created_at, status,
          CASE WHEN status='started' THEN description_started
               WHEN status='aborted' THEN description_aborted
               WHEN status='run-complete-awaiting-finalization' THEN description_awaiting_finalization
               WHEN status='finalized' THEN description_finalized
          END AS description
          , num_jobs_succeeded
          , num_jobs_error
          , last_run_at
          , completed_or_aborted_at
          , finalized_at
          , last_error_at
          , last_error
        FROM codd._background_jobs;

CREATE FUNCTION codd.react_to_job_status_change() RETURNS TRIGGER AS $$
DECLARE
  pg_cron_job_name text;
BEGIN
  IF TG_OP='UPDATE' THEN
    IF OLD.status IN ('finalized', 'aborted') AND NEW.status <> OLD.status THEN
      RAISE EXCEPTION 'Cannot change a background job status from finalized or aborted to anything else. You can delete this job entry if you wish.';
    END IF;
    IF OLD.status='run-complete-awaiting-finalization' AND NEW.status <> 'finalized' THEN
      RAISE EXCEPTION 'Cannot change a background job status from run-complete-awaiting-finalization to anything other than finalized. You can delete this job entry and deal with any outstanding DDL cleanup yourself if you wish.';
    END IF;
    IF NEW.status IN ('run-complete-awaiting-finalization', 'aborted') THEN
      NEW.completed_or_aborted_at = CLOCK_TIMESTAMP();
    ELSIF NEW.status = 'finalized' THEN
      NEW.finalized_at = CLOCK_TIMESTAMP();
    END IF;
  END IF;
    
  IF TG_OP='DELETE' OR NEW.status IN ('aborted', 'run-complete-awaiting-finalization', 'finalized') THEN
    FOREACH pg_cron_job_name IN ARRAY OLD.pg_cron_jobs
    LOOP
      IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') AND EXISTS (SELECT FROM cron.job WHERE jobname = pg_cron_job_name) THEN
         PERFORM cron.unschedule(pg_cron_job_name);
      END IF;
    END LOOP;
  END IF;

  IF TG_OP='DELETE' THEN
    RETURN OLD;
  END IF;
    
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- | This function is to be used live/in Production if the pg_cron job(s) is/are interfering with your
-- application and you want to stop them.
-- It will not apply any DDL; its only side-effect is to stop the job's scheduled pg_cron jobs.
-- After aborting a job, you can delete its row from codd.background_migrations and apply any
-- cleanup DDL yourself, and then retry if you wish.
CREATE FUNCTION codd.abort_background_job(job_name text) RETURNS VOID AS $$
DECLARE
  jobstatus text;
BEGIN
  NOTIFY "codd.___require_codd_schema_channel";
  SELECT status INTO jobstatus FROM codd._background_jobs WHERE jobname=job_name;
  IF jobstatus IS NULL THEN
    RAISE EXCEPTION 'Codd background job named % does not exist', job_name;
  ELSIF jobstatus IN ('run-complete-awaiting-finalization', 'finalized') THEN
    RAISE EXCEPTION 'Codd background job named % already finalized or awaiting finalization, so aborting it would have no effect', job_name;
  END IF;
  UPDATE codd._background_jobs SET status='aborted' WHERE jobname=job_name;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER react_to_status_change
    BEFORE UPDATE OF status
    ON codd._background_jobs
    FOR EACH ROW
    WHEN (OLD.status <> NEW.status)
    EXECUTE FUNCTION codd.react_to_job_status_change();
CREATE TRIGGER react_to_job_deletion
    BEFORE DELETE
    ON codd._background_jobs
    FOR EACH ROW
    EXECUTE FUNCTION codd.react_to_job_status_change();

CREATE FUNCTION codd._assert_job_can_be_created(job_name text, cron_schedule text, plpgsql_to_run_periodically text) RETURNS VOID AS $func$
BEGIN
  IF job_name IS NULL THEN
    RAISE EXCEPTION 'Please supply a job name';
  END IF;
  IF cron_schedule IS NULL THEN
    RAISE EXCEPTION 'Please supply a cron schedule. It could be e.g. "5 seconds" to run every 5 seconds or "0 10 * * *" for every day at 10AM.';
  END IF;
  IF plpgsql_to_run_periodically IS NULL THEN
    RAISE EXCEPTION 'Please supply a body of valid SQL or plpgsql to run on the supplied schedule';
  END IF;
  IF EXISTS (SELECT FROM codd._background_jobs WHERE jobname=job_name) THEN
    RAISE EXCEPTION 'Codd background job named % already exists. Please choose another name or clean up the codd._background_jobs table by deleting successful jobs if that would help', job_name;
  END IF;
  IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') AND EXISTS (SELECT FROM cron.job WHERE jobname=job_name) THEN
    RAISE EXCEPTION 'There already exists a pg_cron job named %. Please choose another name or clean up the the list of pg_cron jobs', job_name;
  END IF;
END;
$func$ LANGUAGE plpgsql;

CREATE FUNCTION codd._append_semi_colon(plpgsqltext text) RETURNS TEXT AS $$
BEGIN
    RETURN CASE WHEN plpgsqltext ~ ';\s*$' THEN plpgsqltext ELSE plpgsqltext || ';' END;
END;
$$ IMMUTABLE STRICT PARALLEL SAFE LANGUAGE plpgsql;
CREATE TYPE codd.succeeded_signal_kind AS ENUM ('when-modifies-0-rows', 'select-into-status-var');

CREATE FUNCTION codd.background_job_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text, description_started text, description_aborted text, description_awaiting_finalization text, description_finalized text, succeeded_signal codd.succeeded_signal_kind = 'when-modifies-0-rows') RETURNS VOID AS $func$
DECLARE
  temp_bg_success_func_name text := format('%I.%I', current_schema, '_codd_job_' || jobname);
  temp_bg_wrapper_func_name text := format('%I.%I', current_schema, '_codd_job_wrapper_' || jobname);
  created_job codd._background_jobs;
BEGIN
  NOTIFY "codd.___require_codd_schema_channel";
  PERFORM codd._assert_worker_is_setup();
  PERFORM codd._assert_job_can_be_created(jobname, cron_schedule, plpgsql_to_run_periodically);

  INSERT INTO codd._background_jobs (jobname, job_function, objects_to_drop_in_order, pg_cron_jobs, description_started, description_aborted, description_awaiting_finalization, description_finalized) VALUES (jobname, temp_bg_wrapper_func_name, ARRAY[ROW('FUNCTION', temp_bg_wrapper_func_name, NULL)::codd.obj_to_drop, ROW('FUNCTION', temp_bg_success_func_name, NULL)::codd.obj_to_drop], ARRAY[jobname], description_started, COALESCE(description_aborted, 'You may delete this row from codd._background_jobs at any time with no side effects'), description_awaiting_finalization, COALESCE(description_finalized, 'Job completed successfully. You may delete this row from codd._background_jobs at any time with no side effects')) RETURNING * INTO created_job;

  -- Why two functions instead of just one? Because each function's effects are atomic, as if
  -- there was a savepoint around each function call, and that enables us to update codd._background_jobs
  -- even if the job errors out. Still, write a test or look for official docs to assert this property.
  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS TEXT AS $$
        DECLARE
          affected_row_count bigint;
          new_codd_job_status TEXT;
        BEGIN
          %s
          %s
          RETURN new_codd_job_status;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$, temp_bg_success_func_name, codd._append_semi_colon(plpgsql_to_run_periodically),
        CASE WHEN succeeded_signal='when-modifies-0-rows' THEN $$
          GET DIAGNOSTICS affected_row_count = row_count;
          new_codd_job_status = CASE WHEN affected_row_count=0 THEN 'run-complete-awaiting-finalization' END;$$ ELSE '' END);

  EXECUTE format($sqltext$
        CREATE FUNCTION %s () RETURNS VOID AS $$
        DECLARE
          new_codd_job_status TEXT;
          stack text;
        BEGIN
            PERFORM * FROM codd._background_jobs WHERE jobname=%s FOR NO KEY UPDATE;
            SELECT %s() INTO new_codd_job_status;
            -- TODO: Can unscheduling cancel the job and rollback the changes applied in the last run? Check. https://github.com/citusdata/pg_cron/issues/308 suggests it might be possible.
            UPDATE codd._background_jobs SET
                num_jobs_succeeded=num_jobs_succeeded+1
              , last_run_at=CLOCK_TIMESTAMP()
              , status=COALESCE(new_codd_job_status, status)
              WHERE jobname=%s;
        EXCEPTION WHEN OTHERS THEN
          -- The EXCEPTION clause silences the error from the logs (understandably) so it's important we
          -- emit at least a warning (we can't re-raise EXCEPTION or this function's UPDATE won't have an effect)
          GET DIAGNOSTICS stack = PG_CONTEXT;
          RAISE WARNING 'Error in codd background job. %% %%. Stack: %%', SQLSTATE, SQLERRM, stack;
          PERFORM * FROM codd._background_jobs WHERE jobname=%s FOR NO KEY UPDATE;
          UPDATE codd._background_jobs SET
              num_jobs_error=num_jobs_error + 1
            , last_run_at=CLOCK_TIMESTAMP()
            , last_error_at=CLOCK_TIMESTAMP()
            , last_error=format('%%s: %%s', SQLSTATE, SQLERRM)
            WHERE jobname=%s;
        END;
        $$ LANGUAGE plpgsql;
  $sqltext$, temp_bg_wrapper_func_name, quote_literal(jobname), temp_bg_success_func_name, quote_literal(jobname), quote_literal(jobname), quote_literal(jobname));

  IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') THEN
    PERFORM cron.schedule(jobname, cron_schedule, format('%s; SELECT %s(); COMMIT;',
                                                      CASE WHEN created_job.txn_isolation_level = 'read-uncommitted' THEN 'BEGIN READ WRITE,ISOLATION LEVEL READ UNCOMMITTED'
                                                           WHEN created_job.txn_isolation_level = 'read-committed' THEN 'BEGIN READ WRITE,ISOLATION LEVEL READ COMMITTED'
                                                           WHEN created_job.txn_isolation_level = 'repeatable-read' THEN 'BEGIN READ WRITE,ISOLATION LEVEL REPEATABLE READ'
                                                           WHEN created_job.txn_isolation_level = 'serializable' THEN 'BEGIN READ WRITE,ISOLATION LEVEL SERIALIZABLE'

                                                           ELSE 'BEGIN READ WRITE' END
                                                      , temp_bg_wrapper_func_name));
  END IF;
END;
$func$ LANGUAGE plpgsql;
-- | Synchronously runs a job until it finalized or until the supplied timeout elapses, and raises an exception in case of the latter.
-- This does nothing if the job does not exist or is already finalized.
-- This doesn't run the job if it has been aborted.
-- This drops database objects created by the job if it completes successfully or if the job was aborted.
-- This does update codd._background_jobs table with the count of every successful and error run until either the timeout or successful completion.
-- This will run the job in the isolation level of the caller's (yours) in a single transaction regardless of how many times the job needs to run.
-- If the timeout elapses and the job isn't finalized, this function will raise an exception, and will therefore fail to update even codd._background_jobs properly.
-- This will drop the auxiliary functions created by codd if it completes successfully.
CREATE FUNCTION codd.synchronously_finalize_background_job(job_name text, timeout interval) RETURNS VOID AS $func$
DECLARE
  start_time timestamptz := clock_timestamp();
  end_time timestamptz := clock_timestamp() + timeout;
  jobrow codd._background_jobs;
  jobstatus text;
  obj_to_drop codd.obj_to_drop;
BEGIN
  NOTIFY "codd.___require_codd_schema_channel";
  PERFORM codd._assert_worker_is_setup();
  IF job_name IS NULL THEN
    RAISE EXCEPTION 'Please supply a job name';
  END IF;
  IF timeout IS NULL THEN
    RAISE EXCEPTION 'Please supply a timeout for synchronously completing a job';
  END IF;

  SELECT * INTO jobrow FROM codd._background_jobs WHERE jobname=job_name;
  IF jobrow.jobname IS NULL OR jobrow.status='finalized' THEN
    RETURN;
  ELSIF jobrow.status = 'aborted' THEN
    RAISE EXCEPTION 'It is not possible to finalize the aborted job %. Please delete the aborted job row from codd.background_migrations and do any cleanup necessary', job_name;
  END IF;

  -- Ensure no competition between this and some job that starts after it.
  PERFORM * FROM codd._background_jobs WHERE jobname=job_name FOR NO KEY UPDATE;
  jobstatus = jobrow.status;
  WHILE jobstatus NOT IN ('aborted', 'run-complete-awaiting-finalization', 'finalized') LOOP
    EXECUTE format('SELECT %s()', jobrow.job_function);
    -- TODO: Make function above return new status to avoid this extra query?
    SELECT status INTO jobstatus FROM codd._background_jobs WHERE jobname=job_name;
    IF clock_timestamp() >= end_time AND jobstatus NOT IN ('run-complete-awaiting-finalization', 'finalized') THEN
      RAISE EXCEPTION 'Codd was unable to synchronously finalize the background job % in the supplied time limit. The job has not been aborted.', job_name;
    END IF;
  END LOOP;
  FOREACH obj_to_drop IN ARRAY jobrow.objects_to_drop_in_order
  LOOP
    EXECUTE format('DROP %s IF EXISTS %s %s', (obj_to_drop).kind, (obj_to_drop).objname, CASE WHEN (obj_to_drop).on_ IS NULL THEN '' ELSE format('ON %s', (obj_to_drop).on_) END);
  END LOOP;
  UPDATE codd._background_jobs SET status='finalized' WHERE jobname=job_name;
END;
$func$ LANGUAGE plpgsql;

-- | Adds triggers and a background job to populate a column with values such that eventually
-- every existing and future row in the table will have values as you define.
-- At that point, the status of the job will be 'run-complete-awaiting-finalization'. You can then
-- finalize the migration with `synchronously_finalize_background_job`, which will drop the triggers and
-- functions created here.
-- It is up to you however to be careful and deploy an application that no longer needs any old columns
-- that the gradually populated column might be replacing before finalizing the job created here.
CREATE FUNCTION codd.populate_column_gradually(job_name text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, colname text, new_col_trigger_expr text) RETURNS VOID AS $func$
DECLARE
  trigger_name text;
  triggfn_name text;
  qualif_table_name text;
BEGIN
  PERFORM codd._assert_worker_is_setup();
  IF tablename IS NULL OR colname IS NULL OR new_col_trigger_expr IS NULL THEN
    RAISE EXCEPTION $err$
Did you forget to supply some arguments to populate_column_gradually? Here is an usage example that updates one row every second:

      ALTER TABLE animals ADD COLUMN new_number INT;
      SELECT codd.populate_column_gradually('new-column-with-old-column-plus1', '1 seconds',
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
  PERFORM codd._assert_job_can_be_created(job_name, cron_schedule, plpgsql_to_run_periodically);

  trigger_name := format('%I', '_codd_bgjob_trig' || job_name);
  triggfn_name := format('%I.%I', current_schema, '_codd_bgjob_' || job_name);
  qualif_table_name := format('%I.%I', current_schema, tablename);
  
  -- TODO: Scheduling before creating the triggers might make the schedule task run before the triggers are created and return 0? Seems unlikely but pg_cron does execute things in separate connections. Oh, in that case the column might not even exist!
  --       So yeah, schedule with pg_cron AFTER creating the triggers! 
  -- TODO: Do BEFORE triggers override values explicitly defined in the VALUES list? We probably want them to?
  -- TODO: Should we forbid changing values of the new column with a trigger? Or does the BEFORE trigger make such attempts futile?
  -- TODO: Add tests with various search_paths to check we're being diligent
  -- TODO: Add tests with various weird object names
  PERFORM codd.background_job_begin(job_name, cron_schedule, plpgsql_to_run_periodically, format('Gradually populating values in the %I.%I column', tablename, colname), format('Given up populating values in the %I.%I column. You can DELETE this job row from codd._background_jobs without any side-effects and do any DDL you deem necessary now', tablename, colname), format('Every row in table %I now has the %I column populated and pg_cron jobs are no longer running. You can now call synchronously_finalize_background_job to remove the triggers and accessory functions created to keep it up-to-date', tablename, colname), NULL);
  UPDATE codd._background_jobs SET objects_to_drop_in_order=ARRAY[ROW('TRIGGER', trigger_name, qualif_table_name)::codd.obj_to_drop, ROW('FUNCTION', triggfn_name, NULL)::codd.obj_to_drop] || objects_to_drop_in_order WHERE jobname=job_name;
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
  $triggers$, triggfn_name, colname, codd._append_semi_colon(new_col_trigger_expr), trigger_name, qualif_table_name, triggfn_name);
END;
$func$ LANGUAGE plpgsql; |]
