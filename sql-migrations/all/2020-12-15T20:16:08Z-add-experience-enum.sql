-- codd: no-txn

BEGIN TRANSACTION READ WRITE;
CREATE TYPE experience AS ENUM ('junior', 'senior');
ALTER TABLE employee ADD COLUMN experience experience;
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
COMMIT;

BEGIN TRANSACTION READ WRITE;
UPDATE employee SET experience='intern';
COMMIT;