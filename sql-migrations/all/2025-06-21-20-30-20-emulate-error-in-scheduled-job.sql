-- codd: requires-codd-schema
-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive.
CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');
ALTER TABLE employee ADD COLUMN experience2 experience2;
SELECT codd.update_table_gradually('change-experience', '1 seconds', 'public.employee',
$$
UPDATE employee SET experience2=CASE WHEN ((RANDOM() * 100)::int % 5) <= 3 THEN (experience::text || '-invalid')::experience2 WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END
    WHERE employee_id=(SELECT employee_id FROM employee WHERE (experience IS NULL) <> (experience2 IS NULL) LIMIT 1);
$$
, $$NEW.experience2=CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$
);
