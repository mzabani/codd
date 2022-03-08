-- Repopulate the table without columns with COPY just to test an edge case!!
DELETE FROM no_cols_table;
COPY no_cols_table FROM STDIN WITH (FORMAT CSV);





\.
