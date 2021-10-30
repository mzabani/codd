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

SELECT '[1.234, 5.678]'::floatrange;

CREATE FUNCTION time_subtype_diff(x time, y time) RETURNS float8 AS
'SELECT EXTRACT(EPOCH FROM (x - y))' LANGUAGE sql STRICT IMMUTABLE;

CREATE TYPE timerange AS RANGE (
    subtype = time,
    subtype_diff = time_subtype_diff
);

SELECT '[11:10, 23:00]'::timerange;
