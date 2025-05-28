CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');
ALTER TABLE employee ADD COLUMN experience2 experience2;
SELECT codd.populate_column_gradually('change-experience', '1 seconds',
$$
UPDATE employee SET experience2=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END
    WHERE employee_id=(SELECT employee_id FROM employee WHERE (experience IS NULL) <> (experience2 IS NULL) LIMIT 1);
$$
, 'employee', 'experience2', $$CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$
);
