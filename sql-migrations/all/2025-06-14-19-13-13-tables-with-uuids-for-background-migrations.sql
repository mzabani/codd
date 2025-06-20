CREATE TABLE unique_uuids (uuid_key UUID NOT NULL PRIMARY KEY);
CREATE TABLE ref_uuids (uuid_key UUID NOT NULL REFERENCES unique_uuids(uuid_key), other_col INT);
INSERT INTO unique_uuids SELECT gen_random_uuid() from generate_series(1,1000);
INSERT INTO ref_uuids SELECT uuid_key FROM unique_uuids, generate_series(1,100);
