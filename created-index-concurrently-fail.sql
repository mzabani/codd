-- This index will fail to be created due to duplicated experiences
SELECT codd_schema.create_index_concurrently('concurrent-index', '10 seconds', 'CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS employee_experience_idx ON employee (experience)', 'employee', 'employee_experience_idx', '1 second');

