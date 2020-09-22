-- codd: non-destructive

-- README:
-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps
-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.

-- 1. Add the column and set its values, initially
ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.
UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name;
ALTER TABLE employee ADD CONSTRAINT employee_rename_equal_ck CHECK (employee_name IS NOT DISTINCT FROM name);

-- 2. Both old and new Apps need to update the two columns consistently until one of them is removed.
CREATE FUNCTION employee_name_rename_set_new() RETURNS TRIGGER AS $$
    BEGIN
        NEW.employee_name = NEW.name;
        RETURN NEW;
    END
$$ LANGUAGE plpgsql;
CREATE FUNCTION employee_name_rename_set_old() RETURNS TRIGGER AS $$
    BEGIN
        NEW.name = NEW.employee_name;
        RETURN NEW;
    END
$$ LANGUAGE plpgsql;

-- 3. Triggers to set the new column name when the old App does something
CREATE TRIGGER employee_old_app_update_column_name
    BEFORE UPDATE ON employee
    FOR EACH ROW
    WHEN (OLD.name IS DISTINCT FROM NEW.name)
    EXECUTE FUNCTION employee_name_rename_set_new();
CREATE TRIGGER employee_old_app_insert_column_name
    BEFORE INSERT ON employee
    FOR EACH ROW
    WHEN (NEW.employee_name IS NULL)
    EXECUTE FUNCTION employee_name_rename_set_new();

-- 4. Triggers to set the old column name when the new App does something
CREATE TRIGGER employee_new_app_update_column_name
    BEFORE UPDATE ON employee
    FOR EACH ROW
    WHEN (OLD.employee_name IS DISTINCT FROM NEW.employee_name)
    EXECUTE FUNCTION employee_name_rename_set_old();
CREATE TRIGGER employee_new_app_insert_column_name
    BEFORE INSERT ON employee
    FOR EACH ROW
    WHEN (NEW.name IS NULL)
    EXECUTE FUNCTION employee_name_rename_set_old();

-- 5. You might want to create indices to speed up your queries here

-- codd: destructive
DROP TRIGGER employee_old_app_update_column_name ON employee;
DROP TRIGGER employee_old_app_insert_column_name ON employee;
DROP TRIGGER employee_new_app_update_column_name ON employee;
DROP TRIGGER employee_new_app_insert_column_name ON employee;
DROP FUNCTION employee_name_rename_set_new();
DROP FUNCTION employee_name_rename_set_old();

-- We drop the new and rename because in the non-destructive section we didn't add constraints that might exist
-- to the new column, but we still want them.
ALTER TABLE employee DROP COLUMN employee_name;
ALTER TABLE employee RENAME COLUMN name TO employee_name;