CREATE TABLE transactions (id BIGINT GENERATED BY DEFAULT AS IDENTITY, payer TEXT NOT NULL, receiver TEXT NOT NULL, value NUMERIC(10,2) CHECK (value > 0));