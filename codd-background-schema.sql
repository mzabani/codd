CREATE TABLE IF NOT EXISTS codd_schema.background_migrations (name text not null primary key);
-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_migration_begin(background_migration_name text, sqlToRunOnce text, cronSchedule text, sqlToRunPeriodically text, runInsideTxn boolean = true) RETURNS VOID AS $func$
DECLARE
  temp_bg_func_name text := 'temp_bg_migration_' || background_migration_name;
BEGIN
  -- TODO: Check for NULLs and throw
  -- TODO: Call function that checks that pg_cron exists and is set up properly and raises error or info
  -- TODO: Check the user calling this function is the same that create codd_schema? Users can only view jobs
  --       in `cron.job` that were created by them due to a RLS policy.
  -- Note that cron.job_run_details might not be turned on.. so we can't rely on it!

  -- Insert into table first so there are no side effects if this is a duplicate name
  -- TODO: is all SQL executed inside a function atomic even outside a txn? Check.
  INSERT INTO codd_schema.background_migrations (name) VALUES (background_migration_name);

  EXECUTE format($sqltext$
        CREATE FUNCTION codd_schema.%I () RETURNS VOID AS $$
        DECLARE
          affected_row_count bigint;
        BEGIN
          %s
          GET DIAGNOSTICS affected_row_count = row_count;
          IF affected_row_count = 0 THEN
            -- TODO: Can unscheduling cancel the job and rollback the changes applied in the last run? Check.
            PERFORM cron.unschedule('%s');
          END IF;
          RETURN;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$ , temp_bg_func_name, sqlToRunPeriodically, background_migration_name);
  EXECUTE sqlToRunOnce;
  -- TODO: Does pg_cron run its statements inside transactions? And how do we control the isolation level?
  PERFORM cron.schedule(background_migration_name, cronSchedule, format('SELECT codd_schema.%I()', temp_bg_func_name));
END;
$func$ LANGUAGE plpgsql;

