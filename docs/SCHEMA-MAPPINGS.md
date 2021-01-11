# Schema Objects and how they are mapped in Codd

A complete list of all columns that affect our DB hashing algorithm can be found in this document.
The rationale behind not including a column is that we don't want/need to include columns that satisfy any of these criteria:

- Are DB-instance-dependent: plain OIDs always satisfy this.
- Are already accounted for in a hierarchical on-disk ancestor or descendant.
- Affect naming of folders and/or files in the on-disk hashes.
- Are redundant, i.e. at least one other column will always change when the ignored column changes.
- Most importantly: do not affect any possible query's results for the Application User. On-Disk size of objects, user's password and others fall in this category.


**IMPORTANT:** Not all features are currently perfectly mapped. Search this document for "TODO" to find missing/imperfect mappings.

## Schemas

This comes from https://www.postgresql.org/docs/12/catalog-pg-namespace.html

### Columns included

OidColumn PgAuthId "nspowner",  
"nspacl"  

### Ignored columns

-- oid	oid	 	Row identifier  
-- nspname	name	 	Name of the namespace  

## Tables, VIEWs, et al.

This comes from https://www.postgresql.org/docs/12/catalog-pg-class.html

### Columns included

OidColumn PgType "reltype",  
OidColumn PgType "reloftype",  
OidColumn PgAuthId "relowner",  
OidColumn PgAccessMethod "relam",  
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

For VIEWs only:
- "pg_views.definition"

### Ignored columns

-- oid	oid	 	Row identifier  
-- relname	name	 	Name of the table, index, view, etc.  
-- relnamespace	oid	pg_namespace.oid	The OID of the namespace that contains this relation  
-- relfilenode	oid	 	Name of the on-disk file of this relation; zero means this is a “mapped” relation whose disk file name is determined by low-level state  
-- reltablespace	oid	pg_tablespace.oid	The tablespace in which this relation is stored. If zero, the database's default tablespace is implied. (Not   meaningful if the relation has no on-disk file.)  
-- relpages	int4	 	Size of the on-disk representation of this table in pages (of size BLCKSZ). This is only an estimate used by the planner. It is updated   by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.  
-- reltuples	float4	 	Number of live rows in the table. This is only an estimate used by the planner. It is updated by VACUUM, ANALYZE, and a few DDL   commands such as CREATE INDEX.  
-- relallvisible	int4	 	Number of pages that are marked all-visible in the table's visibility map. This is only an estimate used by the planner. It is   updated by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.  
-- reltoastrelid	oid	pg_class.oid	OID of the TOAST table associated with this table, 0 if none. The TOAST table stores large attributes “out of line” in a   secondary table.  
-- relhasindex	bool	 	True if this is a table and it has (or recently had) any indexes  
-- relnatts	int2	 	Number of user columns in the relation (system columns not counted). There must be this many corresponding entries in pg_attribute. See   also pg_attribute.attnum.  
-- relchecks	int2	 	Number of CHECK constraints on the table; see pg_constraint catalog  
-- relhasrules	bool	 	True if table has (or once had) rules; see pg_rewrite catalog  
-- relhastriggers	bool	 	True if table has (or once had) triggers; see pg_trigger catalog  
-- relhassubclass	bool	 	True if table or index has (or once had) any inheritance children  
-- relispopulated	bool	 	True if relation is populated (this is true for all relations other than some materialized views)  
-- relrewrite	oid	pg_class.oid	For new relations being written during a DDL operation that requires a table rewrite, this contains the OID of the original   relation; otherwise 0. That state is only visible internally; this field should never contain anything other than 0 for a user-visible relation.  
-- relfrozenxid	xid	 	All transaction IDs before this one have been replaced with a permanent (“frozen”) transaction ID in this table. This is used to track   whether the table needs to be vacuumed in order to prevent transaction ID wraparound or to allow pg_xact to be shrunk. Zero (InvalidTransactionId) if the   relation is not a table.  
-- relminmxid	xid	 	All multixact IDs before this one have been replaced by a transaction ID in this table. This is used to track whether the table needs to   be vacuumed in order to prevent multixact ID wraparound or to allow pg_multixact to be shrunk. Zero (InvalidMultiXactId) if the relation is not a table.

## Sequences

This comes from https://www.postgresql.org/docs/12/catalog-pg-sequence.html

### Columns included

OidColumn PgType "seqtypid",  
"seqstart",  
"seqincrement",  
"seqmax",  
"seqmin",  
"seqcache",  
"seqcycle"  

Plus, all columns we include from pg_class are also included for sequences.

### Ignored columns

- seqrelid	oid	pg_class.oid	The OID of the pg_class entry for this sequence


### TODO

- Table that owns Sequence is not hashed.  
- The `RESTART` value of the sequence also isn't hashed.  

## Columns, Constraints/Indices and other Table-related attributes

This comes from https://www.postgresql.org/docs/12/catalog-pg-attribute.html

### Columns included

OidColumn PgType "atttypid",  
"attnotnull",  
PureSqlExpression "SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND   pg_attrdef.adnum=pg_attribute.attnum" -- This is what we use instead of "atthasdef", which is described as: This column has a default expression or generation   expression, in which case there will be a corresponding entry in the pg_attrdef catalog that actually defines the expression. (Check attgenerated to determine   whether this is a default or a generation expression.)  
"atthasmissing",  
"attidentity",  
"attgenerated",  
"attislocal",  
"attinhcount",  
OidColumn PgCollation "attcollation",  
"attacl",  
"attoptions",  
"attfdwoptions",  
"attmissingval"  

### Ignored columns

-- attrelid	oid	pg_class.oid	The table this column belongs to  
-- attname	name	 	The column name  
-- attstattarget	int4	 	attstattarget controls the level of detail of statistics accumulated for this column by ANALYZE. A zero value indicates that no   statistics should be collected. A negative value says to use the system default statistics target. The exact meaning of positive values is data type-dependent.   For scalar data types, attstattarget is both the target number of “most common values” to collect, and the target number of histogram bins to create.  
-- attlen	int2	 	A copy of pg_type.typlen of this column's type  
-- attnum	int2	 	The number of the column. Ordinary columns are numbered from 1 up. System columns, such as ctid, have (arbitrary) negative numbers.  
-- attndims	int4	 	Number of dimensions, if the column is an array type; otherwise 0. (Presently, the number of dimensions of an array is not enforced, so   any nonzero value effectively means “it's an array”.)  
-- attcacheoff	int4	 	Always -1 in storage, but when loaded into a row descriptor in memory this might be updated to cache the offset of the attribute   within the row  
-- atttypmod	int4	 	atttypmod records type-specific data supplied at table creation time (for example, the maximum length of a varchar column). It is   passed to type-specific input functions and length coercion functions. The value will generally be -1 for types that do not need atttypmod.  
-- attbyval	bool	 	A copy of pg_type.typbyval of this column's type  
-- attstorage	char	 	Normally a copy of pg_type.typstorage of this column's type. For TOAST-able data types, this can be altered after column creation to   control storage policy.  
-- attalign	char	 	A copy of pg_type.typalign of this column's type  
-- attisdropped	bool	 	This column has been dropped and is no longer valid. A dropped column is still physically present in the table, but is ignored by   the parser and so cannot be accessed via SQL.  

### Notes

System columns are created for every table and are _ignored_ by Codd. These are listed in https://www.postgresql.org/docs/12/ddl-system-columns.html.

## Procedures/Functions/Routines

This comes from https://www.postgresql.org/docs/12/catalog-pg-proc.html

### Columns included

OidColumn PgAuthId "proowner",  
OidColumn PgLanguage "prolang",  
OidColumn PgType "provariadic",  
"prokind",  
"prosecdef",  
"proleakproof",  
"proisstrict",  
"proretset",  
"provolatile",  
"proparallel",  
"pronargs",  
"pronargdefaults",  
OidColumn PgType "prorettype",  
OidArrayColumn PgType "proargtypes",  
OidArrayColumn PgType "proallargtypes",  
"proargmodes",  
"proargnames",  
"proargdefaults",  
OidArrayColumn PgType "protrftypes",  
"prosrc",  
"probin",  
"proconfig",  
"proacl"  

### Ignored columns

- oid  
- proname  
- pronamespace  
- procost  
- prorows  

## Constraints

This comes from https://www.postgresql.org/docs/12/catalog-pg-constraint.html

### Columns included

"contype",  
"condeferrable",  
"condeferred",  
"convalidated",  
OidColumn PgClass "conrelid",  
OidColumn PgType "contypid",  
OidColumn PgClass "conindid"  
OidColumn PgConstraint "conparentid",  
OidColumn PgClass "confrelid",  
"confupdtype",  
"confdeltype",  
"confmatchtype",  
"conislocal",  
"coninhcount",  
"connoinherit",  
"conkey", -- TODO: Should we join on pg_attribute and use names of columns instead of their positions?  
"confkey",	-- TODO: Should we join on pg_attribute and use names of columns instead of their positions?  
OidArrayColumn PgOperator "conpfeqop",  
OidArrayColumn PgOperator "conppeqop",  
OidArrayColumn PgOperator "conffeqop",  
OidArrayColumn PgOperator "conexclop",  
"pg_get_constraintdef(pg_constraint.oid)" -- NOTE: The docs recommend using this functions instead of "conbin"  

### Ignored columns

- oid  
- conname  
- connamespace  


## Triggers

This comes from https://www.postgresql.org/docs/12/catalog-pg-trigger.html

### Columns included

OidColumn PgProc "tgfoid",  
"tgtype",  
"tgenabled",  
"tgisinternal",  
OidColumn PgClass "tgconstrrelid",  
OidColumn PgClass "tgconstrindid",  
OidColumn PgConstraint "tgconstraint",  
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

## Roles and role settings

This comes from https://www.postgresql.org/docs/12/catalog-pg-authid.html and https://www.postgresql.org/docs/12/catalog-pg-db-role-setting.html

### Columns included

"rolsuper",  
"rolinherit",  
"rolcreaterole",  
"rolcreatedb",  
"rolcanlogin",  
"rolreplication",  
"rolbypassrls",  
RegularColumn PgRoleSettings "setconfig"  

### Ignored columns

Ignored from pg_authid:

- oid
- rolname
- rolconnlimit: For roles that can log in, this sets maximum number of concurrent connections this role can make. -1 means no limit.
- rolpassword: Password (possibly encrypted); null if none. The format depends on the form of encryption used.
- rolvaliduntil: Password expiry time (only used for password authentication); null if no expiration

Ignored from pg_db_role_setting:

- setdatabase	oid	pg_database.oid	The OID of the database the setting is applicable to, or zero if not database-specific
- setrole	oid	pg_authid.oid	The OID of the role the setting is applicable to, or zero if not role-specific

## Row Level Security Policis

https://www.postgresql.org/docs/12/catalog-pg-policy.html

### Columns included

"polcmd",  
"polpermissive",  
OidArrayColumn PgAuthId "polroles",  
"pg_get_expr(polqual, polrelid)",  
"pg_get_expr(polwithcheck, polrelid)"  

### Ignored columns

- polname: The name of the policy
- polrelid:	The table to which the policy applies