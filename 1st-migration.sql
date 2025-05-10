CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');
ALTER TABLE employee ADD COLUMN experience2 experience2;
CREATE FUNCTION __bgmig_employee_set_experience2() RETURNS TRIGGER AS $$
BEGIN
  NEW.experience2 := CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER __bgmig_employee_set_experience2_on_update
    BEFORE UPDATE OF experience
    ON employee
    EXECUTE FUNCTION __bgmig_employee_set_experience2();
CREATE TRIGGER __bgmig_employee_set_experience2_on_insert
    BEFORE INSERT
    ON employee
    EXECUTE FUNCTION __bgmig_employee_set_experience2();
SELECT codd_schema.background_migration_begin('change-experience', '1 seconds',
$$
UPDATE employee SET experience2=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END
    WHERE employee_id=(SELECT employee_id FROM employee WHERE (experience IS NULL) <> (experience2 IS NULL) LIMIT 1);
$$
);
