CREATE TABLE somedata (id INT NOT NULL, UNIQUE(id) DEFERRABLE INITIALLY DEFERRED);

COPY somedata FROM STDIN WITH (FORMAT csv);
1
2
3
\.
