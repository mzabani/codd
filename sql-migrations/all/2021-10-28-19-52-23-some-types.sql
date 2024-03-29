CREATE DOMAIN non_empty_text TEXT NOT NULL CHECK (VALUE != '');
CREATE DOMAIN non_whitespace_text TEXT NOT NULL CHECK (TRIM(VALUE) != '');

ALTER DOMAIN non_empty_text ADD CONSTRAINT new_constraint CHECK(VALUE != 'empty') NOT VALID;
GRANT USAGE ON DOMAIN non_empty_text TO codd_low_privilege_user;

CREATE TYPE complex AS (
    a       double precision,
    b       double precision
);

CREATE TYPE floatrange AS RANGE (
    subtype = float8,
    subtype_diff = float8mi
);
-- Range constructor functions are created as owned by the database admin,
-- which is not what we want. However, users may not want to run migrations
-- as the SQL admin, and that's the scenario we want to emulate here
-- for development purposes, so that we can discover these restrictions.
-- ALTER ROUTINE floatrange(float8, float8) OWNER TO codd_admin;
-- ALTER ROUTINE floatrange(float8, float8, text) OWNER TO codd_admin;

SELECT '[1.234, 5.678]'::floatrange;

CREATE FUNCTION time_subtype_diff(x time, y time) RETURNS float8 AS
'SELECT EXTRACT(EPOCH FROM (x - y))' LANGUAGE sql STRICT IMMUTABLE;

CREATE TYPE timerange AS RANGE (
    subtype = time,
    subtype_diff = time_subtype_diff
);
-- ALTER ROUTINE timerange(time, time) OWNER TO codd_admin;
-- ALTER ROUTINE timerange(time, time, text) OWNER TO codd_admin;

SELECT '[11:10, 23:00]'::timerange;
