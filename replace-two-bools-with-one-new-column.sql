-- Imagine two boolean columns already existed..
ALTER TABLE employee ADD COLUMN very_experienced bool, ADD COLUMN very_immature bool, ADD CHECK (NOT (very_experienced AND very_immature) AND (very_experienced IS NULL) = (very_immature IS NULL));
UPDATE employee SET
    very_experienced=experience IN ('master', 'senior')
    , very_immature=experience='intern'
    WHERE experience IS NOT NULL;

-- .. and will be replaced by a single new column
CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');
ALTER TABLE employee ADD COLUMN experience2 experience2;
SELECT codd_schema.new_gradual_column('change-experience', '1 seconds',
$$
UPDATE employee SET experience2=CASE WHEN very_experienced THEN 'senior'::experience2 WHEN very_immature THEN 'intern' WHEN very_experienced IS NOT NULL THEN 'junior' END
    WHERE employee_id=(SELECT employee_id FROM employee WHERE (very_experienced IS NULL) <> (experience2 IS NULL) LIMIT 1);
$$
, 'employee', 'experience2', $$CASE WHEN NEW.very_experienced THEN 'senior' WHEN NEW.very_immature THEN 'intern' WHEN NEW.very_experienced IS NOT NULL THEN 'junior' END$$
);

