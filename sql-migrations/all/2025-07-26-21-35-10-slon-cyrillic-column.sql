-- Adds a column with a special Unicode escape character. Taken from https://www.postgresql.org/docs/current/sql-syntax-lexical.html
ALTER TABLE employee ADD COLUMN U&"\0441\043B\043E\043D" TEXT;
UPDATE employee SET U&"\0441\043B\043E\043D"='slon in cyrillic';
SELECT U&"!0441!043B!043E!043D" UESCAPE '!' FROM employee;
