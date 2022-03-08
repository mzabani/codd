-- codd: no-txn
ALTER TYPE experience ADD VALUE 'master' AFTER 'senior';
UPDATE employee SET experience='master';