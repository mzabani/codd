CREATE TABLE no_cols_table (x INT);
INSERT INTO no_cols_table (x) VALUES (1), (2), (3), (4), (5);
ALTER TABLE no_cols_table DROP COLUMN x;