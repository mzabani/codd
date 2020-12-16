-- codd: no-txn
CREATE TYPE experience AS ENUM ('junior', 'senior');
ALTER TABLE employee ADD COLUMN experience experience;
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET experience='intern';