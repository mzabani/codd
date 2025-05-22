SELECT codd_schema.create_index_concurrently('concurrent-index', '10 seconds', 'CREATE INDEX CONCURRENTLY IF NOT EXISTS employee_experience_idx ON employee (experience)', 'employee', 'employee_experience_idx', '1 second');

