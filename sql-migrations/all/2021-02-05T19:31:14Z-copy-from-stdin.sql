-- COPY employee FROM STDIN WITH (FORMAT CSV);
-- "5","Dracula","master"
-- "6","The Grinch","master"
-- \.
SELECT 'just for fun';

COPY employee FROM STDIN WITH (FORMAT CSV);
5,Dracula,master
6,The Grinch,master
\.

SELECT setval('employee_employee_id_seq', (SELECT MAX(employee_id) FROM employee));