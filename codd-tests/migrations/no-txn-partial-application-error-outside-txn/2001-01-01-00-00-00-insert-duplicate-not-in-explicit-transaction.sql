-- codd: no-txn
INSERT INTO somedata(id) VALUES (4); -- This statement would fail if codd tries to rerun it

COPY somedata FROM STDIN WITH (FORMAT csv);
1
2
3
\.
