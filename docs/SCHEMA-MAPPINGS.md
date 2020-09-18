# Schema Objects and how they are mapped in Codd

1. Procedures/Functions/Routines

This comes from the full list of columns at https://www.postgresql.org/docs/12/catalog-pg-proc.html.

## Columns included

JoinOid "pronamespace" "pg_namespace",
JoinOid "proowner" "pg_authid",
JoinOid "prolang" "pg_language",
"procost",
"prorows",
JoinOid "provariadic" "pg_type",
"prokind",
"prosecdef",
"proleakproof",
"proisstrict",
"proretset",
"provolatile",
"proparallel",
"pronargs",
"pronargdefaults",
JoinOid "prorettype" "pg_type",
JoinOidArray "proargtypes" "pg_type",
"proargmodes",
"proargnames",
"proargdefaults",
JoinOidArray "protrftypes" "pg_type",
"prosrc",
"probin",
"proconfig",
"proacl"

## Ignored columns

These are either already accounted for in a hierarchical on-disk ancestor (such as namespaces) or seem to be redundant.

- proallargtypes: An array with the data types of the function arguments. This includes all arguments (including OUT and INOUT arguments); however, if all the arguments are IN arguments, this field will be null. Note that subscripting is 1-based, whereas for historical reasons proargtypes is subscripted from 0.