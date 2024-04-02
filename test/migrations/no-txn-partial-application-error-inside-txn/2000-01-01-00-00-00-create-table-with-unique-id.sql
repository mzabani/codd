-- codd: no-txn
CREATE TABLE somedata (id INT NOT NULL, UNIQUE(id));

COPY somedata FROM STDIN WITH (FORMAT csv);
1
2
3
\.
