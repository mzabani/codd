CREATE SCHEMA collations_1;
CREATE SCHEMA collations_2;
CREATE COLLATION collations_1.pt_br_test_coll (provider = icu, locale = 'pt-u-co-phonebk');
CREATE COLLATION collations_2.pt_br_test_coll (provider = libc, locale = 'pt_BR.utf8');

ALTER TABLE employee ADD COLUMN surname TEXT COLLATE collations_1.pt_br_test_coll;