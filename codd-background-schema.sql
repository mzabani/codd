CREATE TABLE IF NOT EXISTS codd_schema.background_migrations (jobname TEXT NOT NULL PRIMARY KEY, status TEXT NOT NULL DEFAULT 'started', last_error TEXT, num_jobs_succeeded INT NOT NULL DEFAULT 0, job_func_name TEXT NOT NULL, CHECK (status IN ('started', 'aborted', 'finished')));
CREATE FUNCTION codd_schema.drop_codd_job_function() RETURNS TRIGGER AS $$
BEGIN
  -- TODO: Catch error if the job doesn't exist or check if it's there before unscheduling to avoid
  -- an error from pg_cron!
   IF EXISTS (
      SELECT FROM cron.job WHERE jobname = OLD.jobname) THEN

      PERFORM cron.unschedule(OLD.jobname);
   END IF;
  -- TODO: Can a function safely drop itself? Or is it fine if this runs in the context of the trigger?
  EXECUTE format($drop$DROP FUNCTION IF EXISTS codd_schema.%I$drop$, OLD.job_func_name);
  IF TG_OP='DELETE' THEN
    RETURN OLD;
  END IF;
    
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE FUNCTION codd_schema.abort_background_migration(jobname text) RETURNS VOID AS $$
BEGIN
  UPDATE codd_schema.background_migrations SET status='aborted' WHERE background_migrations.jobname=jobname;
END;
$$ LANGUAGE plpgsql STRICT;
CREATE TRIGGER drop_job_func_on_update
    BEFORE UPDATE OF status
    ON codd_schema.background_migrations
    FOR EACH ROW
    WHEN (OLD.status NOT IN ('aborted', 'finished') AND NEW.status IN ('aborted', 'finished'))
    EXECUTE FUNCTION codd_schema.drop_codd_job_function();
CREATE TRIGGER drop_job_func_on_delete
    BEFORE UPDATE OF status
    ON codd_schema.background_migrations
    FOR EACH ROW
    WHEN (OLD.status NOT IN ('aborted', 'finished'))
    EXECUTE FUNCTION codd_schema.drop_codd_job_function();

-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_migration_begin(jobname text, cron_schedule text, plpgsql_to_run_periodically text, runInsideTxn boolean = true) RETURNS VOID AS $func$
DECLARE
  temp_bg_func_name text := 'tmp_bg_migration_' || jobname;
BEGIN
  -- TODO: Check for NULLs and throw
  -- TODO: Call function that checks that pg_cron exists and is set up properly and raises error or info
  -- TODO: Check the user calling this function is the same that create codd_schema? Users can only view jobs
  --       in `cron.job` that were created by them due to a RLS policy.
  -- Note that cron.job_run_details might not be turned on.. so we can't rely on it!

  -- Insert into table first so there are no side effects if this is a duplicate name
  -- TODO: is all SQL executed inside a function atomic even outside a txn? Check. And honor `runInsideTxn`
  INSERT INTO codd_schema.background_migrations (jobname, job_func_name) VALUES (jobname, temp_bg_func_name);

  -- TODO: Test very long names for migrations (more than 63 chars).
  EXECUTE format($sqltext$
        CREATE FUNCTION codd_schema.%I () RETURNS VOID AS $$
        DECLARE
          affected_row_count bigint;
        BEGIN
          -- TODO: Catch error in user statement and set `last_error`. Maybe add a `num_errors` column too?
          %s
          GET DIAGNOSTICS affected_row_count = row_count;
          UPDATE codd_schema.background_migrations SET num_jobs_succeeded=num_jobs_succeeded+1 WHERE jobname='%s';
          IF affected_row_count = 0 THEN
            -- TODO: Can unscheduling cancel the job and rollback the changes applied in the last run? Check. https://github.com/citusdata/pg_cron/issues/308 suggests it might be possible.
            UPDATE codd_schema.background_migrations SET status='finished' WHERE jobname='%s';
          END IF;
          RETURN;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$ , temp_bg_func_name, plpgsql_to_run_periodically, jobname, jobname);

  -- TODO: Does pg_cron run its statements inside transactions? And how do we control the isolation level?
  PERFORM cron.schedule(jobname, cron_schedule, format('SELECT codd_schema.%I()', temp_bg_func_name));
END;
$func$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION codd_schema.new_gradual_column(jobname text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, oldcolnames text[], newcolname text, new_col_update_trigger_expr text, runInsideTxn boolean = true) RETURNS VOID AS $func$
DECLARE
  trigger_base_name text := '_codd_bgmig_' || tablename || '_gradual_col_' || newcolname;
  oldcolname text;
  comma_sep_old_cols text := NULL;
BEGIN
  -- TODO: Check for a bunch of things like the other function does
  -- TODO: Check old and new column exist, tell user the new column must be created by them before calling this function
  -- TODO: Scheduling before creating the triggers might make the schedule task run before the triggers are created and return 0? Seems unlikely but pg_cron does execute things in separate connections. Oh, in that case the column might not even exist!
  --       So yeah, schedule with pg_cron AFTER creating the triggers! 
  -- TODO: Do BEFORE triggers override values explicitly defined in the VALUES list? We probably want them to?
  -- TODO: Do we forbid changing values of the new column with a trigger?
  PERFORM codd_schema.background_migration_begin(jobname, cron_schedule, plpgsql_to_run_periodically);
  FOREACH oldcolname IN ARRAY oldcolnames
  LOOP
    comma_sep_old_cols = CASE WHEN comma_sep_old_cols IS NULL THEN format('%I', oldcolname)
                                                              ELSE comma_sep_old_cols || ', ' || format('%I', oldcolname) END;
  END LOOP;
  EXECUTE format($triggers$
  CREATE FUNCTION %I() RETURNS TRIGGER AS $$
  BEGIN
    NEW.%I := %s;
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  CREATE TRIGGER %I
      BEFORE UPDATE OF %s
      ON %I
      EXECUTE FUNCTION %I();
  CREATE TRIGGER %I
      BEFORE INSERT
      ON %I
      EXECUTE FUNCTION %I();
  $triggers$, trigger_base_name, newcolname, new_col_update_trigger_expr, 'upd' || trigger_base_name, comma_sep_old_cols, tablename, trigger_base_name, 'ins' || trigger_base_name, tablename, trigger_base_name);
END;
$func$ LANGUAGE plpgsql;

