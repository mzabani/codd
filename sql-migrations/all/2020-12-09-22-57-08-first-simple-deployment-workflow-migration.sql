-- DISCLAIMER: this migration is for the purposes of testing and developing codd, and it may refer to features
-- that no longer exist or practices that are no longer recommended. Do not assume things you find in this file
-- are representative of codd's current state or that it represents good practices.

CREATE TABLE bonus_pay (
    employee_id INT NOT NULL REFERENCES employee(employee_id)
    , value NUMERIC(10,2) CHECK (value > 0)
    , value_copy NUMERIC(10,2) CHECK (value_copy > 0)
);

-- Just something destructive, which should not matter for simple deployment workflow
ALTER TABLE bonus_pay DROP COLUMN value_copy;