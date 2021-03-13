# Schema Objects and how they are mapped in Codd

A complete list of all columns that affect our DB hashing algorithm can be found in this document.
The rationale behind not including a column is that we don't want/need to include columns that satisfy any of these criteria:

- Are DB-instance-dependent: plain OIDs always satisfy this. When a OID column appears included in this document, it's the referenced object's name that we hash.  
- Are already accounted for in a hierarchical on-disk ancestor or descendant.
- Affect naming of folders and/or files in the on-disk hashes.
- Are redundant, i.e. at least one other column will always change when the ignored column changes.
- Do not affect any possible query's results for the Application User. On-Disk size of objects, user's password and others fall in this category.

## Other notes

- We still use system functions from https://www.postgresql.org/docs/13/functions-info.html to checksum objects' contents some times. However, we want to remove all usage of functions that return object definitions in text form. The reasoning is that even slightly changing what those functions return (maybe even spacing) would likely not be considered breaking behaviour by Postgres devs, but it would break _codd_.  
  - Whether these functions are more reliable than `::TEXT` representations of objects' definitions, though...  

**IMPORTANT:** Not all features are currently perfectly mapped. Search this document for "TODO" to find missing/imperfect mappings.

## Schemas

This comes from https://www.postgresql.org/docs/12/catalog-pg-namespace.html

### Columns included

"nspowner",  
"nspacl"  

### Ignored columns

-- oid	oid	 	Row identifier  
-- nspname	name	 	Name of the namespace  

## Tables, VIEWs, et al.

This comes from https://www.postgresql.org/docs/12/catalog-pg-class.html

### Columns included

"reltype",  
"reloftype",  
"relowner",  
"relam",  
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

"seqtypid",  
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

## Columns, Constraints and other Table-related attributes

This comes from https://www.postgresql.org/docs/12/catalog-pg-attribute.html

### Columns included

"atttypid",  
"attnotnull",  
PureSqlExpression "SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND   pg_attrdef.adnum=pg_attribute.attnum" -- This is what we use instead of "atthasdef", which is described as: This column has a default expression or generation   expression, in which case there will be a corresponding entry in the pg_attrdef catalog that actually defines the expression. (Check attgenerated to determine   whether this is a default or a generation expression.)  
"atthasmissing",  
"attidentity",  
"attgenerated",  
"attislocal",  
"attinhcount",  
"attcollation",  
"attacl",  
"attoptions",  
"attfdwoptions",  
"attmissingval",
"attnum"

### Ignored columns

-- attrelid	oid	pg_class.oid	The table this column belongs to  
-- attname	name	 	The column name  
-- attstattarget	int4	 	attstattarget controls the level of detail of statistics accumulated for this column by ANALYZE. A zero value indicates that no   statistics should be collected. A negative value says to use the system default statistics target. The exact meaning of positive values is data type-dependent.   For scalar data types, attstattarget is both the target number of “most common values” to collect, and the target number of histogram bins to create.  
-- attlen	int2	 	A copy of pg_type.typlen of this column's type  
-- attndims	int4	 	Number of dimensions, if the column is an array type; otherwise 0. (Presently, the number of dimensions of an array is not enforced, so   any nonzero value effectively means “it's an array”.)  
-- attcacheoff	int4	 	Always -1 in storage, but when loaded into a row descriptor in memory this might be updated to cache the offset of the attribute   within the row  
-- atttypmod	int4	 	atttypmod records type-specific data supplied at table creation time (for example, the maximum length of a varchar column). It is   passed to type-specific input functions and length coercion functions. The value will generally be -1 for types that do not need atttypmod.  
-- attbyval	bool	 	A copy of pg_type.typbyval of this column's type  
-- attstorage	char	 	Normally a copy of pg_type.typstorage of this column's type. For TOAST-able data types, this can be altered after column creation to   control storage policy.  
-- attalign	char	 	A copy of pg_type.typalign of this column's type  
-- attisdropped	bool	 	This column has been dropped and is no longer valid. A dropped column is still physically present in the table, but is ignored by   the parser and so cannot be accessed via SQL.  

### Notes

System columns are created for every table and are _ignored_ by Codd. These are listed in https://www.postgresql.org/docs/12/ddl-system-columns.html.

## Indexes

This comes from https://www.postgresql.org/docs/11/catalog-pg-index.html

### Columns included


"indisunique",	bool	 	If true, this is a unique index
"indisprimary",	bool	 	If true, this index represents the primary key of the table (indisunique should always be true when this is true)
"indisexclusion",	bool	 	If true, this index supports an exclusion constraint
"indimmediate",	bool	 	If true, the uniqueness check is enforced immediately on insertion (irrelevant if indisunique is not true)
"indkey",	int2vector	pg_attribute.attnum	This is an array of indnatts values that indicate which table columns this index indexes. For example a value of 1 3 would mean that the first and the third table columns make up the index entries. Key columns come before non-key (included) columns. A zero in this array indicates that the corresponding index attribute is an expression over the table columns, rather than a simple column reference.
"indcollation",	oidvector	pg_collation.oid	For each column in the index key (indnkeyatts values), this contains the OID of the collation to use for the index, or zero if the column is not of a collatable data type.
"indclass",	oidvector	pg_opclass.oid	For each column in the index key (indnkeyatts values), this contains the OID of the operator class to use. See pg_opclass for details.
"indoption",	int2vector	 	This is an array of indnkeyatts values that store per-column flag bits. The meaning of the bits is defined by the index's access method.
"indexprs",	pg_node_tree	 	Expression trees (in nodeToString() representation) for index attributes that are not simple column references. This is a list with one element for each zero entry in indkey. Null if all index attributes are simple references.
"indpred",	pg_node_tree	 	Expression tree (in nodeToString() representation) for partial index predicate. Null if not a partial index.

### Ignored columns

"indexrelid",	oid	pg_class.oid	The OID of the pg_class entry for this index
"indrelid",	oid	pg_class.oid	The OID of the pg_class entry for the table this index is for
"indnatts",	int2	 	The total number of columns in the index (duplicates pg_class.relnatts); this number includes both key and included attributes
"indnkeyatts",	int2	 	The number of key columns in the index, not counting any included columns, which are merely stored and do not participate in the index semantics
"indisclustered",	bool	 	If true, the table was last clustered on this index
"indisvalid",	bool	 	If true, the index is currently valid for queries. False means the index is possibly incomplete: it must still be modified by INSERT/UPDATE operations, but it cannot safely be used for queries. If it is unique, the uniqueness property is not guaranteed true either.
indcheckxmin	bool	 	If true, queries must not use the index until the xmin of this pg_index row is below their TransactionXmin event horizon, because the table may contain broken HOT chains with incompatible rows that they can see
indisready	bool	 	If true, the index is currently ready for inserts. False means the index must be ignored by INSERT/UPDATE operations.
indislive	bool	 	If false, the index is in process of being dropped, and should be ignored for all purposes (including HOT-safety decisions)
indisreplident	bool	 	If true this index has been chosen as “replica identity” using ALTER TABLE ... REPLICA IDENTITY USING INDEX ...
## Procedures/Functions/Routines

This comes from https://www.postgresql.org/docs/12/catalog-pg-proc.html

### Columns included

"proowner",  
"prolang",  
"prokind",  
"prosecdef",  
"proleakproof",  
"proisstrict",  
"proretset",  
"provolatile",  
"proparallel",  
"pronargs",  
"pronargdefaults",  
"prorettype",  
"proargtypes",  
"prosrc", but only when the language of the function is NOT internal  
"proargmodes",  
"proargnames",  
"proargdefaults",  
"proconfig",  
"proacl"  

### Ignored columns

- oid  
- proname  
- pronamespace  
- procost  
- prorows  
- provariadic  
- proallargtypes  
- protrftypes  
- prosrc, but only when the language of the function is internal
- probin (ignored because of linked symbols might differ across machines?)  

## Constraints

This comes from https://www.postgresql.org/docs/12/catalog-pg-constraint.html

### Columns included

"contype",  
"condeferrable",  
"condeferred",  
"convalidated",  
"conrelid",  
"contypid",  
"conindid"  
"conparentid",  
"confrelid",  
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

"tgfoid",  
"tgtype",  
"tgenabled",  
"tgisinternal",  
"tgconstrrelid",  
"tgconstrindid",  
"tgconstraint",  
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

This comes from https://www.postgresql.org/docs/12/catalog-pg-authid.html, https://www.postgresql.org/docs/12/catalog-pg-auth-members.html and even https://www.postgresql.org/docs/12/catalog-pg-database.html due to its `dataacl` column.

### Columns included

"rolsuper",  
"rolinherit",  
"rolcreaterole",  
"rolcreatedb",  
"rolcanlogin",  
"rolreplication",  
"rolbypassrls",  
"setconfig"  

.. also others from the other tables

### Ignored columns

Ignored from pg_authid:

- oid
- rolname
- rolconnlimit: For roles that can log in, this sets maximum number of concurrent connections this role can make. -1 means no limit.
- rolpassword: Password (possibly encrypted); null if none. The format depends on the form of encryption used.
- rolvaliduntil: Password expiry time (only used for password authentication); null if no expiration
- 
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