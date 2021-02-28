DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_low_privilege_user') THEN

      CREATE USER "codd_low_privilege_user";
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO "codd_low_privilege_user";

REVOKE ALL ON TABLE employee FROM codd_low_privilege_user;