-- Emulate removing 'master' from an enum type and switching every 'master' to 'senior'
-- RUN IN FOREGROUND IN THE 1st MIGRATION:
CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');

-- RUN IN BACKGROUND ONCE in 1st MIGRATION:
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

-- RUN IN BACKGROUND PERIODICALLY UNTIL THIS RETURNS "UPDATE 0", SCHEDULED WITH 1st MIGRATION:
SELECT cron.schedule('1 seconds', $$
UPDATE employee SET experience2=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END
    WHERE employee_id=(SELECT employee_id FROM employee WHERE (experience IS NULL) <> (experience2 IS NULL) LIMIT 1);
$$);


-- FINISH THE CHANGE WITH 2nd MIGRATION:
-- TODO: If someone renames the table the naming convention stops working!
-- Maybe just drop the column CASCADE?
DROP TRIGGER __bgmig_employee_set_experience2_on_update ON employee;
DROP TRIGGER __bgmig_employee_set_experience2_on_insert ON employee;
DROP FUNCTION __bgmig_employee_set_experience2;
ALTER TABLE employee DROP COLUMN experience;
DROP TYPE experience;
ALTER TYPE experience2 RENAME TO experience;
ALTER TABLE employee RENAME COLUMN experience2 TO experience;
