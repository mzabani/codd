module Codd.InternalSchema.V5 (migrateInternalSchemaV4ToV5) where

import Codd.Query (InTxn, execvoid_)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO)

migrateInternalSchemaV4ToV5 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
migrateInternalSchemaV4ToV5 conn = do
  execvoid_
    conn
    "ALTER SCHEMA codd_schema RENAME TO codd;\n\
    \CREATE TABLE codd._background_worker_type (\n\
    \  id INT NOT NULL PRIMARY KEY CHECK (id = 1) -- Enforce a single row\n\
    \  , worker_type TEXT NOT NULL CHECK (worker_type IN ('pg_cron', 'external'))\n\
    \);\n\
    \GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE codd._background_worker_type TO PUBLIC;\n\
    \CREATE FUNCTION codd.setup_background_worker(worker_type TEXT) RETURNS VOID AS $$\n\
    \BEGIN\n\
    \  NOTIFY \"codd.___require_codd_schema_channel\";\n\
    \  IF worker_type NOT IN ('pg_cron', 'external') THEN\n\
    \    RAISE EXCEPTION 'Background workers supported by codd are either \"pg_cron\", which requires the extension, or \"external\", which requires you to set up an external job runner yourself to periodically run scheduled jobs.';\n\
    \  END IF;\n\
    \  IF worker_type = 'pg_cron' AND NOT EXISTS (SELECT FROM pg_catalog.pg_extension WHERE extname='pg_cron') THEN\n\
    \    RAISE EXCEPTION 'Setting up codd background migrations with pg_cron requires the pg_cron extension to be installed first. Please check https://github.com/citusdata/pg_cron for installation instructions';\n\
    \  END IF;\n\
    \\n\
    \  -- For now we don't let users switch the job runner if there are any active jobs\n\
    \  IF EXISTS (SELECT FROM codd._background_worker_type) AND (SELECT _background_worker_type.worker_type FROM codd._background_worker_type) <> worker_type AND EXISTS (SELECT FROM codd._background_jobs WHERE status NOT IN ('aborted', 'finalized')) THEN\n\
    \    RAISE EXCEPTION 'You cannot change your background worker with codd.setup_background_worker without finalizing or aborting all existing jobs. Check the codd.abort_background_job and codd.synchronously_finalize_background_job functions for how to do that and query the codd.jobs view to see active jobs';\n\
    \  END IF;\n\
    \\n\
    \  INSERT INTO codd._background_worker_type (id, worker_type) VALUES (1, worker_type)\n\
    \  ON CONFLICT (id) DO UPDATE SET worker_type=EXCLUDED.worker_type;\n\
    \END;\n\
    \$$ LANGUAGE plpgsql;\n\
    \CREATE FUNCTION codd._assert_worker_is_setup() RETURNS VOID AS $$\n\
    \BEGIN\n\
    \  NOTIFY \"codd.___require_codd_schema_channel\";\n\
    \  IF NOT EXISTS (SELECT FROM codd._background_worker_type) THEN\n\
    \    RAISE EXCEPTION 'You must call the codd.setup_background_worker function before scheduling jobs. You can either add `SELECT codd.setup_background_worker(''pg_cron'')` or `SELECT codd.setup_background_worker(''external'')` to your migration depending on whether you would like the pg_cron extension to run periodic jobs or if you wish to implement a job runner yourself otherwise';\n\
    \  END IF;\n\
    \END;\n\
    \$$ LANGUAGE plpgsql;\n\
    \CREATE TYPE codd._obj_to_drop AS (\n\
    \    kind text,\n\
    \    objname text,\n\
    \    on_ text\n\
    \);\n\
    \CREATE TABLE codd._background_jobs (\n\
    \  jobid SERIAL PRIMARY KEY,\n\
    \  jobname TEXT NOT NULL,\n\
    \  status TEXT NOT NULL DEFAULT 'started',\n\
    \  created_at TIMESTAMPTZ NOT NULL DEFAULT CLOCK_TIMESTAMP(),\n\
    \  created_by_role TEXT NOT NULL DEFAULT current_user,\n\
    \  txn_isolation_level TEXT NOT NULL DEFAULT COALESCE(CURRENT_SETTING('codd.___txn_isolation_level', TRUE), 'db-default'),\n\
    \  last_run_at TIMESTAMPTZ,\n\
    \  last_error_at TIMESTAMPTZ,\n\
    \  completed_or_aborted_at TIMESTAMPTZ,\n\
    \  finalized_at TIMESTAMPTZ,\n\
    \  num_jobs_succeeded BIGINT NOT NULL DEFAULT 0,\n\
    \  num_jobs_error BIGINT NOT NULL DEFAULT 0,\n\
    \  last_error TEXT,\n\
    \  description_started TEXT,\n\
    \  description_aborted TEXT NOT NULL DEFAULT 'You may delete this row from codd._background_jobs at any time with no side effects',\n\
    \  description_awaiting_finalization TEXT,\n\
    \  description_finalized TEXT NOT NULL DEFAULT 'Job completed successfully. You may delete this row from codd._background_jobs at any time with no side effects',\n\
    \  job_function TEXT NOT NULL,\n\
    \  objects_to_drop_in_order codd._obj_to_drop[] NOT NULL,\n\
    \  pg_cron_jobs TEXT[] NOT NULL\n\
    \  , UNIQUE (jobname)\n\
    \  , CHECK (txn_isolation_level IN ('db-default', 'read-uncommitted', 'read-committed', 'repeatable-read', 'serializable'))\n\
    \  , CHECK (status IN ('started', 'aborted', 'run-complete-awaiting-finalization', 'finalized'))\n\
    \  , CHECK (completed_or_aborted_at IS NULL OR status <> 'started')\n\
    \  , CHECK ((finalized_at IS NOT NULL) = (status = 'finalized'))\n\
    \);\n\
    \GRANT INSERT,SELECT,UPDATE,DELETE ON TABLE codd._background_jobs TO PUBLIC;\n\
    \CREATE VIEW codd.jobs AS\n\
    \  SELECT jobname, created_at, status,\n\
    \          CASE WHEN status='started' THEN description_started\n\
    \               WHEN status='aborted' THEN description_aborted\n\
    \               WHEN status='run-complete-awaiting-finalization' THEN description_awaiting_finalization\n\
    \               WHEN status='finalized' THEN description_finalized\n\
    \          END AS description\n\
    \          , num_jobs_succeeded\n\
    \          , num_jobs_error\n\
    \          , last_run_at\n\
    \          , completed_or_aborted_at\n\
    \          , finalized_at\n\
    \          , last_error_at\n\
    \          , last_error\n\
    \        FROM codd._background_jobs;\n\
    \\n\
    \CREATE FUNCTION codd._react_to_job_status_change() RETURNS TRIGGER AS $$\n\
    \DECLARE\n\
    \  pg_cron_job_name text;\n\
    \BEGIN\n\
    \  IF TG_OP='UPDATE' THEN\n\
    \    IF OLD.status IN ('finalized', 'aborted') AND NEW.status <> OLD.status THEN\n\
    \      RAISE EXCEPTION 'Cannot change a background job status from finalized or aborted to anything else. You can delete this job entry if you wish.';\n\
    \    END IF;\n\
    \    IF OLD.status='run-complete-awaiting-finalization' AND NEW.status <> 'finalized' THEN\n\
    \      RAISE EXCEPTION 'Cannot change a background job status from run-complete-awaiting-finalization to anything other than finalized. You can delete this job entry and deal with any outstanding DDL cleanup yourself if you wish.';\n\
    \    END IF;\n\
    \    IF NEW.status IN ('run-complete-awaiting-finalization', 'aborted') THEN\n\
    \      NEW.completed_or_aborted_at = CLOCK_TIMESTAMP();\n\
    \    ELSIF NEW.status = 'finalized' THEN\n\
    \      NEW.finalized_at = CLOCK_TIMESTAMP();\n\
    \    END IF;\n\
    \  END IF;\n\
    \    \n\
    \  IF TG_OP='DELETE' OR NEW.status IN ('aborted', 'run-complete-awaiting-finalization', 'finalized') THEN\n\
    \    FOREACH pg_cron_job_name IN ARRAY OLD.pg_cron_jobs\n\
    \    LOOP\n\
    \      IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') THEN\n\
    \        IF EXISTS (SELECT FROM cron.job WHERE jobname = pg_cron_job_name) THEN\n\
    \         PERFORM cron.unschedule(pg_cron_job_name);\n\
    \        END IF;\n\
    \      END IF;\n\
    \    END LOOP;\n\
    \  END IF;\n\
    \\n\
    \  IF TG_OP='DELETE' THEN\n\
    \    RETURN OLD;\n\
    \  END IF;\n\
    \    \n\
    \  RETURN NEW;\n\
    \END;\n\
    \$$ LANGUAGE plpgsql;\n\
    \\n\
    \CREATE FUNCTION codd.abort_background_job(job_name text) RETURNS VOID AS $$\n\
    \DECLARE\n\
    \  jobstatus text;\n\
    \BEGIN\n\
    \  NOTIFY \"codd.___require_codd_schema_channel\";\n\
    \  SELECT status INTO jobstatus FROM codd._background_jobs WHERE jobname=job_name;\n\
    \  IF jobstatus IS NULL THEN\n\
    \    RAISE EXCEPTION 'Codd background job named % does not exist', job_name;\n\
    \  ELSIF jobstatus IN ('run-complete-awaiting-finalization', 'finalized') THEN\n\
    \    RAISE EXCEPTION 'Codd background job named % already finalized or awaiting finalization, so aborting it would have no effect', job_name;\n\
    \  END IF;\n\
    \  UPDATE codd._background_jobs SET status='aborted' WHERE jobname=job_name;\n\
    \END;\n\
    \$$ LANGUAGE plpgsql;\n\
    \CREATE TRIGGER react_to_status_change\n\
    \    BEFORE UPDATE OF status\n\
    \    ON codd._background_jobs\n\
    \    FOR EACH ROW\n\
    \    WHEN (OLD.status <> NEW.status)\n\
    \    EXECUTE FUNCTION codd._react_to_job_status_change();\n\
    \CREATE TRIGGER react_to_job_deletion\n\
    \    BEFORE DELETE\n\
    \    ON codd._background_jobs\n\
    \    FOR EACH ROW\n\
    \    EXECUTE FUNCTION codd._react_to_job_status_change();\n\
    \\n\
    \CREATE FUNCTION codd._assert_job_can_be_created(job_name text, cron_schedule text, plpgsql_to_run_periodically text) RETURNS VOID AS $func$\n\
    \BEGIN\n\
    \  IF job_name IS NULL THEN\n\
    \    RAISE EXCEPTION 'Please supply a job name';\n\
    \  END IF;\n\
    \  IF cron_schedule IS NULL THEN\n\
    \    RAISE EXCEPTION 'Please supply a cron schedule. It could be e.g. \"5 seconds\" to run every 5 seconds or \"0 10 * * *\" for every day at 10AM.';\n\
    \  END IF;\n\
    \  IF plpgsql_to_run_periodically IS NULL THEN\n\
    \    RAISE EXCEPTION 'Please supply a body of valid SQL or plpgsql to run on the supplied schedule';\n\
    \  END IF;\n\
    \  IF EXISTS (SELECT FROM codd._background_jobs WHERE jobname=job_name) THEN\n\
    \    RAISE EXCEPTION 'Codd background job named % already exists. Please choose another name or clean up the codd._background_jobs table by deleting successful jobs if that would help', job_name;\n\
    \  END IF;\n\
    \  IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') THEN\n\
    \    IF EXISTS (SELECT FROM cron.job WHERE jobname=job_name) THEN\n\
    \      RAISE EXCEPTION 'There already exists a pg_cron job named %. Please choose another name or clean up the the list of pg_cron jobs', job_name;\n\
    \    END IF;\n\
    \  END IF;\n\
    \END;\n\
    \$func$ LANGUAGE plpgsql;\n\
    \\n\
    \CREATE FUNCTION codd._append_semi_colon(plpgsqltext text) RETURNS TEXT AS $$\n\
    \BEGIN\n\
    \    RETURN CASE WHEN plpgsqltext ~ ';\\s*$' THEN plpgsqltext ELSE plpgsqltext || ';' END;\n\
    \END;\n\
    \$$ IMMUTABLE STRICT PARALLEL SAFE LANGUAGE plpgsql;\n\
    \CREATE TYPE codd.succeeded_signal_kind AS ENUM ('when-modifies-0-rows', 'select-into-status-var');\n\
    \\n\
    \CREATE FUNCTION codd.background_job_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text, description_started text, description_aborted text, description_awaiting_finalization text, description_finalized text, succeeded_signal codd.succeeded_signal_kind = 'when-modifies-0-rows') RETURNS VOID AS $func$\n\
    \DECLARE\n\
    \  temp_bg_success_func_name text := format('%I.%I', current_schema, '_codd_job_' || jobname);\n\
    \  temp_bg_wrapper_func_name text := format('%I.%I', current_schema, '_codd_job_wrapper_' || jobname);\n\
    \  created_job codd._background_jobs;\n\
    \BEGIN\n\
    \  PERFORM codd._assert_worker_is_setup();\n\
    \  PERFORM codd._assert_job_can_be_created(jobname, cron_schedule, plpgsql_to_run_periodically);\n\
    \\n\
    \  INSERT INTO codd._background_jobs (jobname, job_function, objects_to_drop_in_order, pg_cron_jobs, description_started, description_aborted, description_awaiting_finalization, description_finalized) VALUES (jobname, temp_bg_wrapper_func_name, ARRAY[ROW('FUNCTION', temp_bg_wrapper_func_name, NULL)::codd._obj_to_drop, ROW('FUNCTION', temp_bg_success_func_name, NULL)::codd._obj_to_drop], ARRAY[jobname], description_started, COALESCE(description_aborted, 'You may delete this row from codd._background_jobs at any time with no side effects'), description_awaiting_finalization, COALESCE(description_finalized, 'Job completed successfully. You may delete this row from codd._background_jobs at any time with no side effects')) RETURNING * INTO created_job;\n\
    \\n\
    \  -- Why two functions instead of just one: Because each function's effects are atomic, as if\n\
    \  -- there was a savepoint around each function call, and that enables us to update codd._background_jobs\n\
    \  -- even if the job errors out. Still, write a test or look for official docs to assert this property.\n\
    \  EXECUTE format($sqltext$\n\
    \        CREATE FUNCTION %s () RETURNS TEXT AS $$\n\
    \        DECLARE\n\
    \          affected_row_count bigint;\n\
    \          new_codd_job_status TEXT;\n\
    \        BEGIN\n\
    \          %s\n\
    \          %s\n\
    \          RETURN new_codd_job_status;\n\
    \        END;\n\
    \        $$ LANGUAGE plpgsql;\n\
    \      $sqltext$, temp_bg_success_func_name, codd._append_semi_colon(plpgsql_to_run_periodically),\n\
    \        CASE WHEN succeeded_signal='when-modifies-0-rows' THEN $$\n\
    \          GET DIAGNOSTICS affected_row_count = row_count;\n\
    \          new_codd_job_status = CASE WHEN affected_row_count=0 THEN 'run-complete-awaiting-finalization' END;$$ ELSE '' END);\n\
    \\n\
    \  EXECUTE format($sqltext$\n\
    \        CREATE FUNCTION %s () RETURNS VOID AS $$\n\
    \        DECLARE\n\
    \          new_codd_job_status TEXT;\n\
    \          stack text;\n\
    \        BEGIN\n\
    \            PERFORM * FROM codd._background_jobs WHERE jobname=%s FOR NO KEY UPDATE;\n\
    \            SELECT %s() INTO new_codd_job_status;\n\
    \            UPDATE codd._background_jobs SET\n\
    \                num_jobs_succeeded=num_jobs_succeeded+1\n\
    \              , last_run_at=CLOCK_TIMESTAMP()\n\
    \              , status=COALESCE(new_codd_job_status, status)\n\
    \              WHERE jobname=%s;\n\
    \        EXCEPTION WHEN OTHERS THEN\n\
    \          -- The EXCEPTION clause silences the error from the logs (understandably) so it's important we\n\
    \          -- emit at least a warning (we can't re-raise EXCEPTION or this function's UPDATE won't have an effect)\n\
    \          GET DIAGNOSTICS stack = PG_CONTEXT;\n\
    \          RAISE WARNING 'Error in codd background job. %% %%. Stack: %%', SQLSTATE, SQLERRM, stack;\n\
    \          PERFORM * FROM codd._background_jobs WHERE jobname=%s FOR NO KEY UPDATE;\n\
    \          UPDATE codd._background_jobs SET\n\
    \              num_jobs_error=num_jobs_error + 1\n\
    \            , last_run_at=CLOCK_TIMESTAMP()\n\
    \            , last_error_at=CLOCK_TIMESTAMP()\n\
    \            , last_error=format('%%s: %%s', SQLSTATE, SQLERRM)\n\
    \            WHERE jobname=%s;\n\
    \        END;\n\
    \        $$ LANGUAGE plpgsql;\n\
    \  $sqltext$, temp_bg_wrapper_func_name, quote_literal(jobname), temp_bg_success_func_name, quote_literal(jobname), quote_literal(jobname), quote_literal(jobname));\n\
    \\n\
    \  IF EXISTS (SELECT FROM codd._background_worker_type WHERE worker_type='pg_cron') THEN\n\
    \    PERFORM cron.schedule(jobname, cron_schedule, format('%s; SELECT %s(); COMMIT;',\n\
    \                                                      CASE WHEN created_job.txn_isolation_level = 'read-uncommitted' THEN 'BEGIN READ WRITE,ISOLATION LEVEL READ UNCOMMITTED'\n\
    \                                                           WHEN created_job.txn_isolation_level = 'read-committed' THEN 'BEGIN READ WRITE,ISOLATION LEVEL READ COMMITTED'\n\
    \                                                           WHEN created_job.txn_isolation_level = 'repeatable-read' THEN 'BEGIN READ WRITE,ISOLATION LEVEL REPEATABLE READ'\n\
    \                                                           WHEN created_job.txn_isolation_level = 'serializable' THEN 'BEGIN READ WRITE,ISOLATION LEVEL SERIALIZABLE'\n\
    \\n\
    \                                                           ELSE 'BEGIN READ WRITE' END\n\
    \                                                      , temp_bg_wrapper_func_name));\n\
    \  END IF;\n\
    \END;\n\
    \$func$ LANGUAGE plpgsql;\n\
    \\n\
    \CREATE FUNCTION codd.synchronously_finalize_background_job(job_name text, timeout interval) RETURNS VOID AS $func$\n\
    \DECLARE\n\
    \  start_time timestamptz := clock_timestamp();\n\
    \  end_time timestamptz := clock_timestamp() + timeout;\n\
    \  jobrow codd._background_jobs;\n\
    \  jobstatus text;\n\
    \  obj_to_drop codd._obj_to_drop;\n\
    \BEGIN\n\
    \  PERFORM codd._assert_worker_is_setup();\n\
    \  IF job_name IS NULL THEN\n\
    \    RAISE EXCEPTION 'Please supply a job name';\n\
    \  END IF;\n\
    \  IF timeout IS NULL THEN\n\
    \    RAISE EXCEPTION 'Please supply a timeout for synchronously completing a job';\n\
    \  END IF;\n\
    \\n\
    \  SELECT * INTO jobrow FROM codd._background_jobs WHERE jobname=job_name;\n\
    \  IF jobrow.jobname IS NULL THEN\n\
    \    RAISE EXCEPTION 'Could not find job %', job_name;\n\
    \  ELSIF jobrow.status='finalized' THEN\n\
    \    RETURN;\n\
    \  ELSIF jobrow.status = 'aborted' THEN\n\
    \    RAISE EXCEPTION 'It is not possible to finalize the aborted job %. Please delete the aborted job row from codd.background_migrations and do any cleanup necessary', job_name;\n\
    \  END IF;\n\
    \\n\
    \  -- Ensure no competition between this and some job that starts after it.\n\
    \  PERFORM * FROM codd._background_jobs WHERE jobname=job_name FOR NO KEY UPDATE;\n\
    \  jobstatus = jobrow.status;\n\
    \  WHILE jobstatus NOT IN ('aborted', 'run-complete-awaiting-finalization', 'finalized') LOOP\n\
    \    EXECUTE format('SELECT %s()', jobrow.job_function);\n\
    \    SELECT status INTO jobstatus FROM codd._background_jobs WHERE jobname=job_name;\n\
    \    IF clock_timestamp() >= end_time AND jobstatus NOT IN ('run-complete-awaiting-finalization', 'finalized') THEN\n\
    \      RAISE EXCEPTION 'Codd was unable to synchronously finalize the background job % in the supplied time limit. The job has not been aborted.', job_name;\n\
    \    END IF;\n\
    \  END LOOP;\n\
    \  FOREACH obj_to_drop IN ARRAY jobrow.objects_to_drop_in_order\n\
    \  LOOP\n\
    \    EXECUTE format('DROP %s IF EXISTS %s %s', (obj_to_drop).kind, (obj_to_drop).objname, CASE WHEN (obj_to_drop).on_ IS NULL THEN '' ELSE format('ON %s', (obj_to_drop).on_) END);\n\
    \  END LOOP;\n\
    \  UPDATE codd._background_jobs SET status='finalized' WHERE jobname=job_name;\n\
    \END;\n\
    \$func$ LANGUAGE plpgsql;\n\
    \\n\
    \CREATE FUNCTION codd.populate_column_gradually(job_name text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, colname text, new_col_trigger_expr text) RETURNS VOID AS $func$\n\
    \DECLARE\n\
    \  trigger_name text;\n\
    \  triggfn_name text;\n\
    \  qualif_table_name text;\n\
    \BEGIN\n\
    \  PERFORM codd._assert_worker_is_setup();\n\
    \  IF tablename IS NULL OR colname IS NULL OR new_col_trigger_expr IS NULL THEN\n\
    \    RAISE EXCEPTION $err$\n\
    \Did you forget to supply some arguments to codd.populate_column_gradually? Here is an usage example that updates one row every second:\n\
    \\n\
    \      ALTER TABLE animals ADD COLUMN new_number INT;\n\
    \      SELECT codd.populate_column_gradually('new-column-with-old-column-plus1', '1 seconds',\n\
    \        $$\n\
    \        UPDATE animals SET new_number=old_number+1 WHERE animal_id=(SELECT animal_id FROM animals WHERE new_number IS NULL LIMIT 1);\n\
    \        $$\n\
    \      , 'animals', 'new_number', 'NEW.old_number+1'\n\
    \      );\n\
    \    $err$;\n\
    \  END IF;\n\
    \  IF NOT EXISTS (SELECT FROM pg_catalog.pg_attribute\n\
    \                        JOIN pg_catalog.pg_class ON attrelid=pg_class.oid\n\
    \                        JOIN pg_catalog.pg_namespace ON pg_class.relnamespace=pg_namespace.oid\n\
    \                        WHERE attname=colname AND relname=tablename AND nspname=current_schema) THEN\n\
    \      RAISE EXCEPTION 'Column % of relation % does not exist in your preferred schema. Please create the column yourself before calling populate_column_gradually, and possibly check your search_paths setting', colname, tablename; \n\
    \  END IF;\n\
    \  PERFORM codd._assert_job_can_be_created(job_name, cron_schedule, plpgsql_to_run_periodically);\n\
    \\n\
    \  trigger_name := format('%I', '_codd_bgjob_trig' || job_name);\n\
    \  triggfn_name := format('%I.%I', current_schema, '_codd_bgjob_' || job_name);\n\
    \  qualif_table_name := format('%I.%I', current_schema, tablename);\n\
    \  \n\
    \  PERFORM codd.background_job_begin(job_name, cron_schedule, plpgsql_to_run_periodically, format('Gradually populating values in the %I.%I column', tablename, colname), format('Given up populating values in the %I.%I column. You can DELETE this job row from codd._background_jobs without any side-effects and do any DDL you deem necessary now', tablename, colname), format('Every row in table %I now has the %I column populated and background jobs are no longer running. You can now call codd.synchronously_finalize_background_job to remove the triggers and accessory functions created to keep the new column up-to-date', tablename, colname), NULL);\n\
    \  UPDATE codd._background_jobs SET objects_to_drop_in_order=ARRAY[ROW('TRIGGER', trigger_name, qualif_table_name)::codd._obj_to_drop, ROW('FUNCTION', triggfn_name, NULL)::codd._obj_to_drop] || objects_to_drop_in_order WHERE jobname=job_name;\n\
    \  EXECUTE format($triggers$\n\
    \  CREATE FUNCTION %s() RETURNS TRIGGER AS $$\n\
    \  BEGIN\n\
    \    NEW.%I = %s\n\
    \    RETURN NEW;\n\
    \  END;\n\
    \  $$ LANGUAGE plpgsql;\n\
    \  CREATE TRIGGER %s\n\
    \      BEFORE UPDATE OR INSERT\n\
    \      ON %s\n\
    \      FOR EACH ROW\n\
    \      EXECUTE FUNCTION %s();\n\
    \  $triggers$, triggfn_name, colname, codd._append_semi_colon(new_col_trigger_expr), trigger_name, qualif_table_name, triggfn_name);\n\
    \END;\n\
    \$func$ LANGUAGE plpgsql;"

-- The code below is for a future codd.create_index_concurrently function. Usage might be something like:
-- SELECT codd.create_index_concurrently('concurrent-index', '10 seconds', 'CREATE INDEX CONCURRENTLY IF NOT EXISTS employee_experience_idx ON employee (experience)', 'employee', 'employee_experience_idx', '1 second');
-- -- | I just want to check this is possible; we don't need it as much as e.g. populating columns and we'll need schema ignore rules before this works nicely with codd, too
-- CREATE OR REPLACE FUNCTION codd.create_index_concurrently(job_name text, check_completion_cron_schedule text, create_index_concurrently_if_not_exists_statement text, tablename text, indexname text, try_create_cron_schedule text) RETURNS VOID AS $func$
-- DECLARE
--   qualif_table_name text;
-- BEGIN
--   -- TODO: Raise exception that disables this function completely until we have schema ignore rules in place to support it well
--   -- TODO: Check that no pg_cron job with the "-try-create" ending exists
--   -- TODO: If there is a schedule for trying (as opposed to just trying once), then we should be able to handle failure well, too.
--   -- TODO: We need to make the DDL change run only once.. things like REINDEX CONCURRENTLY don't have an idempotent form available, and we don't want those to just keep on recreating indexes repeatedly. Maybe for now we ask users to `DROP INDEX` and `CREATE INDEX` again and just document this limitation.
--   PERFORM codd.background_job_begin(job_name, check_completion_cron_schedule,
--     format($$
--       SELECT CASE WHEN COUNT(*)=1 THEN 'finalized' END INTO new_codd_job_status
--       FROM pg_catalog.pg_index
--       JOIN pg_catalog.pg_class index_class ON indexrelid=index_class.oid
--       JOIN pg_catalog.pg_namespace ON relnamespace=pg_namespace.oid
--       JOIN pg_catalog.pg_class index_table ON index_table.oid=indrelid
--       WHERE nspname=%s AND index_class.relname=%s AND index_table.relname=%s AND indisready AND indislive AND indisvalid;$$, quote_literal(current_schema), quote_literal(indexname), quote_literal(tablename))
--     , format('Periodically checking that the %I index on table %I was (concurrently) created successfully and is ready to be used', indexname, tablename), format('Given up on creating the %I index on table %I. You may DELETE this job from codd._background_jobs at any time without side effects', indexname, tablename), NULL, format('Index %I on table %I successfully created. This job may be DELETEd from codd._background_jobs', indexname, tablename), 'select-into-status-var');

--   -- Add another cron job that does not run in a transaction to create the index
--   UPDATE codd._background_jobs
--     SET pg_cron_jobs = pg_cron_jobs || ARRAY[job_name || '-try-create']
--     WHERE jobname=job_name;
--   PERFORM cron.schedule(job_name || '-try-create', try_create_cron_schedule, create_index_concurrently_if_not_exists_statement);
-- END;

-- $func$ LANGUAGE plpgsql;
