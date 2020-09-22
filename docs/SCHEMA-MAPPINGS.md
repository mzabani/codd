# Schema Objects and how they are mapped in Codd

A complete list of all columns that affect our DB hashing algorithm can be found in this document.
The rationale behind not including a column is that we don't want/need to include columns that satisfy any of these criteria:

- Are DB-instance-dependent: plain OIDs always satisfy this.
- Are already accounted for in a hierarchical on-disk ancestor (so that if the ancestor's name changes it already reflects in the final hashes).
- Affect naming of folders and/or files in the on-disk hashes.
- Are redundant, i.e. at least one other column will always change when the ignored column changes.
- Most importantly: do not affect any possible query's results for the Application User. On-Disk size of objects, user's password and others fall in this category.


**IMPORTANT:** Not all features are currently perfectly mapped. Search this document for "TODO" to find missing/imperfect mappings.

## Tables, VIEWs, et al.

This comes from https://www.postgresql.org/docs/12/catalog-pg-class.html

### Columns included

JoinOid PgType "reltype",
JoinOid PgType "reloftype",
JoinOid PgAuthId "relowner",
JoinOid PgAccessMethod "relam",
"relisshared",
"relpersistence",
"relkind",
"relrowsecurity",
"relforcerowsecurity",
"relreplident",
"relispartition",
"relacl",
"reloptions",
"relpartbound"

### Ignored columns

-- oid	oid	 	Row identifier
-- relname	name	 	Name of the table, index, view, etc.
-- relnamespace	oid	pg_namespace.oid	The OID of the namespace that contains this relation
-- relfilenode	oid	 	Name of the on-disk file of this relation; zero means this is a “mapped” relation whose disk file name is determined by low-level state
-- reltablespace	oid	pg_tablespace.oid	The tablespace in which this relation is stored. If zero, the database's default tablespace is implied. (Not meaningful if the relation has no on-disk file.)
-- relpages	int4	 	Size of the on-disk representation of this table in pages (of size BLCKSZ). This is only an estimate used by the planner. It is updated by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.
-- reltuples	float4	 	Number of live rows in the table. This is only an estimate used by the planner. It is updated by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.
-- relallvisible	int4	 	Number of pages that are marked all-visible in the table's visibility map. This is only an estimate used by the planner. It is updated by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.
-- reltoastrelid	oid	pg_class.oid	OID of the TOAST table associated with this table, 0 if none. The TOAST table stores large attributes “out of line” in a secondary table.
-- relhasindex	bool	 	True if this is a table and it has (or recently had) any indexes
-- relnatts	int2	 	Number of user columns in the relation (system columns not counted). There must be this many corresponding entries in pg_attribute. See also pg_attribute.attnum.
-- relchecks	int2	 	Number of CHECK constraints on the table; see pg_constraint catalog
-- relhasrules	bool	 	True if table has (or once had) rules; see pg_rewrite catalog
-- relhastriggers	bool	 	True if table has (or once had) triggers; see pg_trigger catalog
-- relhassubclass	bool	 	True if table or index has (or once had) any inheritance children
-- relispopulated	bool	 	True if relation is populated (this is true for all relations other than some materialized views)
-- relrewrite	oid	pg_class.oid	For new relations being written during a DDL operation that requires a table rewrite, this contains the OID of the original relation; otherwise 0. That state is only visible internally; this field should never contain anything other than 0 for a user-visible relation.
-- relfrozenxid	xid	 	All transaction IDs before this one have been replaced with a permanent (“frozen”) transaction ID in this table. This is used to track whether the table needs to be vacuumed in order to prevent transaction ID wraparound or to allow pg_xact to be shrunk. Zero (InvalidTransactionId) if the relation is not a table.
-- relminmxid	xid	 	All multixact IDs before this one have been replaced by a transaction ID in this table. This is used to track whether the table needs to be vacuumed in order to prevent multixact ID wraparound or to allow pg_multixact to be shrunk. Zero (InvalidMultiXactId) if the relation is not a table.

## Procedures/Functions/Routines

This comes from https://www.postgresql.org/docs/12/catalog-pg-proc.html

### Columns included

JoinOid PgAuthId "proowner",
JoinOid PgLanguage "prolang",
"procost",
"prorows",
JoinOid PgType "provariadic",
"prokind",
"prosecdef",
"proleakproof",
"proisstrict",
"proretset",
"provolatile",
"proparallel",
"pronargs",
"pronargdefaults",
JoinOid PgType "prorettype",
JoinOidArray PgType "proargtypes",
"proargmodes",
"proargnames",
"proargdefaults",
JoinOidArray PgType "protrftypes",
"prosrc",
"probin",
"proconfig",
"proacl"

### Ignored columns

- oid
- pronamespace
- proallargtypes: An array with the data types of the function arguments. This includes all arguments (including OUT and INOUT arguments); however, if all the arguments are IN arguments, this field will be null. Note that subscripting is 1-based, whereas for historical reasons proargtypes is subscripted from 0.


## Constraints

This comes from https://www.postgresql.org/docs/12/catalog-pg-constraint.html

### Columns included

"contype",
"condeferrable",
"condeferred",
"convalidated",
JoinOid PgClass "conrelid",
JoinOid PgType "contypid",
JoinOid PgClass "conindid"
JoinOid PgConstraint "conparentid",
JoinOid PgClass "confrelid",
"confupdtype",
"confdeltype",
"confmatchtype",
"conislocal",
"coninhcount",
"connoinherit",
"conkey", -- TODO: Should we join on pg_attribute and use names of columns instead of their positions?
"confkey",	-- TODO: Should we join on pg_attribute and use names of columns instead of their positions?
JoinOidArray PgOperator "conpfeqop",
JoinOidArray PgOperator "conppeqop",
JoinOidArray PgOperator "conffeqop",
JoinOidArray PgOperator "conexclop",
"pg_get_constraintdef(pg_constraint.oid)" -- NOTE: The docs recommend using this functions instead of "conbin"

### Ignored columns

- oid
- conname
- connamespace


## Triggers

This comes from https://www.postgresql.org/docs/12/catalog-pg-trigger.html

### Columns included

JoinOid PgProc "tgfoid",
"tgtype",
"tgenabled",
"tgisinternal",
JoinOid PgClass "tgconstrrelid",
JoinOid PgClass "tgconstrindid",
JoinOid PgConstraint "tgconstraint",
"tgdeferrable",
"tginitdeferred",
"tgnargs",
"tgattr",
"tgargs",
"tgqual",
"tgoldtable",
"tgnewtable"

### Ignored columns

- oid
- tgname
- tgrelid: The table this trigger is on

## Roles

This comes from https://www.postgresql.org/docs/12/catalog-pg-authid.html

### Columns included

"rolsuper",
"rolinherit",
"rolcreaterole",
"rolcreatedb",
"rolcanlogin",
"rolreplication",
"rolbypassrls"

### Ignored columns

- oid
- rolname
- rolconnlimit: For roles that can log in, this sets maximum number of concurrent connections this role can make. -1 means no limit.
- rolpassword: Password (possibly encrypted); null if none. The format depends on the form of encryption used.
- rolvaliduntil: Password expiry time (only used for password authentication); null if no expiration