-- codd: requires-codd-schema
-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive.
ALTER TABLE unique_uuids ADD COLUMN new_id SERIAL;
CREATE UNIQUE INDEX ON unique_uuids (new_id);
SELECT codd.populate_column_gradually('add-new-key', '50 seconds',
$$
UPDATE unique_uuids SET new_id=DEFAULT WHERE new_id IS NULL
$$
, 'unique_uuids', 'new_id', 'OLD.new_id'
);
SELECT codd.synchronously_finalize_background_job('add-new-key', '1 minute');
DELETE FROM codd._background_jobs WHERE jobname='add-new-key';
ALTER TABLE unique_uuids ALTER COLUMN new_id SET NOT NULL;

ALTER TABLE ref_uuids ADD COLUMN new_id INT REFERENCES unique_uuids(new_id);
SELECT codd.populate_column_gradually('add-new-key', '1 seconds',
$$
UPDATE ref_uuids SET new_id=unique_uuids.new_id
    FROM unique_uuids
    WHERE unique_uuids.uuid_key=ref_uuids.uuid_key AND ref_uuids.uuid_key IN (SELECT uuid_key FROM ref_uuids WHERE new_id IS NULL LIMIT 10000);
$$
, 'ref_uuids', 'new_id', $$(SELECT new_id FROM unique_uuids WHERE unique_uuids.uuid_key=NEW.uuid_key)$$
);

