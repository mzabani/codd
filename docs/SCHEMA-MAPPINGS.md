# Schema Objects and how they are mapped in Codd

A complete list of all columns that affect our DB hashing algorithm can be found in this document.
The rationale behind not including a column is that we don't want/need to include columns that satisfy any of these criteria:

- Are DB-instance-dependent: plain OIDs always satisfy this.
- Are already accounted for in a hierarchical on-disk ancestor (so that if the ancestor's name changes it already reflects in the final hashes).
- Affect naming of folders and/or files in the on-disk hashes.
- Are redundant, i.e. at least one other column will always change when the ignored column changes.
- Most importantly: do not affect any possible query's results for the Application User. On-Disk size of objects, user's password and others fall in this category.


**IMPORTANT:** Not all features are currently perfectly mapped. Search this document for "TODO" to find missing/imperfect mappings.

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