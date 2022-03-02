CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY
    , employee_name TEXT NOT NULL
);
INSERT INTO employee (employee_name) VALUES ('John Doe');