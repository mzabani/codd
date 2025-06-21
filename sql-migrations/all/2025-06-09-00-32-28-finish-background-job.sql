-- codd: requires-codd-schema
-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it.
SELECT codd.synchronously_finalize_background_job('change-experience', '100 seconds')
