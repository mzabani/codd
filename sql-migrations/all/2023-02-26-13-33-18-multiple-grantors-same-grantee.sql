-- codd: no-txn
-- codd-env-vars: PGHOST, PGSUPERUSER, PGPASSWORD, PGDATABASE
-- codd-connection: user=${PGSUPERUSER} password=${PGPASSWORD} host=${PGHOST} dbname=${PGDATABASE}

begin;
    grant select on table employee to "codd-user" with grant option;
    set role "codd-user";
    grant select on table employee to codd_admin;
    grant select on table employee to PUBLIC;
commit;