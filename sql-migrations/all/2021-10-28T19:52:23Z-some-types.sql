CREATE DOMAIN non_empty_text TEXT NOT NULL CHECK (VALUE != '');
CREATE DOMAIN non_whitespace_text TEXT NOT NULL CHECK (TRIM(VALUE) != '');

CREATE TYPE complex AS (
    a       double precision,
    b       double precision
);