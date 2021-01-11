-- CREATE VIEW vista AS SELECT 'Hello World';
CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee);