CREATE TABLE IF NOT EXISTS codd_schema.background_migrations (name text not null primary key);
-- | This function schedules a background migration that must be a valid plpgsql function body. It will until the last statement in that body returns a row count of 0 (zero), and then will stop running.
-- TODO: Is there no better way? The command tag check would be very nice. Maybe we ask to return a single boolean if that's not possible?
CREATE OR REPLACE FUNCTION codd_schema.background_migration_begin(background_migration_name text, cron_schedule text, plpgsql_to_run_periodically text, runInsideTxn boolean = true) RETURNS VOID AS $func$
DECLARE
  temp_bg_func_name text := 'temp_bg_migration_' || background_migration_name;
BEGIN
  -- TODO: Check for NULLs and throw
  -- TODO: Call function that checks that pg_cron exists and is set up properly and raises error or info
  -- TODO: Check the user calling this function is the same that create codd_schema? Users can only view jobs
  --       in `cron.job` that were created by them due to a RLS policy.
  -- Note that cron.job_run_details might not be turned on.. so we can't rely on it!

  -- Insert into table first so there are no side effects if this is a duplicate name
  -- TODO: is all SQL executed inside a function atomic even outside a txn? Check. And honor `runInsideTxn`
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
            -- TODO: Update codd_schema.background_migrations with a 'finished' status
            PERFORM cron.unschedule('%s');
          END IF;
          RETURN;
        END;
        $$ LANGUAGE plpgsql;
      $sqltext$ , temp_bg_func_name, plpgsql_to_run_periodically, background_migration_name);

  -- TODO: Does pg_cron run its statements inside transactions? And how do we control the isolation level?
  PERFORM cron.schedule(background_migration_name, cron_schedule, format('SELECT codd_schema.%I()', temp_bg_func_name));
END;
$func$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION codd_schema.column_replacement_begin(background_migration_name text, cron_schedule text, plpgsql_to_run_periodically text, tablename text, oldcolname text, newcolname text, new_col_update_trigger_expr text, runInsideTxn boolean = true) RETURNS VOID AS $func$
DECLARE
  trigger_base_name text := '_codd_bgmig_' || tablename || '_replace_' || oldcolname || '_' || newcolname;
BEGIN
  -- TODO: Check for a bunch of things like the other function does
  -- TODO: Check old and new column exist, tell user the new column must be created by them before calling this function
  -- TODO: Scheduling before creating the triggers might make the schedule task run before the triggers are created and return 0? Seems unlikely but pg_cron does execute things in separate connections. Oh, in that case the column might not even exist!
  --       So yeah, schedule with pg_cron AFTER creating the triggers! 
  PERFORM codd_schema.background_migration_begin(background_migration_name, cron_schedule, plpgsql_to_run_periodically);
  EXECUTE format($triggers$
  CREATE FUNCTION %I() RETURNS TRIGGER AS $$
  BEGIN
    NEW.%I := %s;
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
  CREATE TRIGGER %I
      BEFORE UPDATE OF %I
      ON %I
      EXECUTE FUNCTION %I();
  CREATE TRIGGER %I
      BEFORE INSERT
      ON %I
      EXECUTE FUNCTION %I();
  $triggers$, trigger_base_name, newcolname, new_col_update_trigger_expr, 'upd' || trigger_base_name, oldcolname, tablename, trigger_base_name, 'ins' || trigger_base_name, tablename, trigger_base_name);
END;
$func$ LANGUAGE plpgsql;

