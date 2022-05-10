-- codd: no-txn
-- codd-connection: postgres://${PGSUPERUSER}@${PGHOST}:${PGPORT}/${PGDATABASE}
-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT , PGDATABASE

ALTER SCHEMA public OWNER TO ${PGUSER};