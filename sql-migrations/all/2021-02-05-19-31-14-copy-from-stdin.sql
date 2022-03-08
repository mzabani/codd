-- COPY employee FROM STDIN WITH (FORMAT CSV);
-- "5","Dracula","master"
-- "6","The Grinch","master"
-- \.
SELECT 'just for fun';

COPY employee FROM STDIN WITH (FORMAT CSV);
5,Dracula,master
6,The Grinch,master
\.

COPY public.employee FROM STDIN WITH (format csv);
7,Frankenstein,junior
8,Medusa,senior
\.

COPY public . employee FROM STDIN WITH (FORMAT CSV);
9,Werewolf,intern
\.

-- One empty COPY
COPY "public"."employee" FROM STDIN WITH (FORMAT CSV);
\.

SELECT setval('employee_employee_id_seq', (SELECT MAX(employee_id) FROM employee));