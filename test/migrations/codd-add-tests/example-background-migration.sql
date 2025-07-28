-- The folder this file lives in ("codd-add-tests") is not writable by anyone; check with `ls -l`.
-- This helps test it being added without throwing an exception in one of our automated tests, without it being removed.
-- We could copy it into a temporary folder, but it's a bonus to also test the condition where the file to be added is
-- not in a folder writable by the user.
SELECT codd.setup_background_worker('external');
CREATE TABLE employee (somecolumn int);
SELECT codd.populate_column_gradually('user-job-name', '1 seconds', 'employee.somecolumn', 'UPDATE employee SET somecolumn=1', '17');
