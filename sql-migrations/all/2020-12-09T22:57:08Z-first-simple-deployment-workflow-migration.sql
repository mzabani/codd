CREATE TABLE bonus_pay (
    employee_id INT NOT NULL REFERENCES employee(employee_id)
    , value NUMERIC(10,2) CHECK (value > 0)
    , value_copy NUMERIC(10,2) CHECK (value_copy > 0)
);

-- Just something destructive, which should not matter for simple deployment workflow
ALTER TABLE bonus_pay DROP COLUMN value_copy;