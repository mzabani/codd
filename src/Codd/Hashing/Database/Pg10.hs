module Codd.Hashing.Database.Pg10 (Pg10(..), CatalogTable(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison(..))
import Codd.Hashing.Types (HashableObject(..))

data Pg10 = Pg10
data CatalogTable = PgNamespace | PgClass | PgProc | PgAuthId | PgLanguage | PgType | PgConstraint | PgOperator | PgAttribute | PgTrigger | PgAccessMethod | PgCollation | PgPolicy deriving stock Show

instance DbVersionHash Pg10 where
    type CatTable Pg10 = CatalogTable
    hashableObjCatalogTable = \case
        HSchema -> (PgNamespace, Nothing)
        HTable -> (PgClass, Just "pg_class.relkind IN ('r', 'f', 'p')")
        HView -> (PgClass, Just "pg_class.relkind IN ('v', 'm')")
        HSequence -> (PgClass, Just "pg_class.relkind = 'S'")
        HRoutine -> (PgProc, Nothing)
        HColumn -> (PgAttribute, Just "NOT pg_attribute.attisdropped")
        HTableConstraint -> (PgConstraint, Nothing)
        HTrigger -> (PgTrigger, Nothing)
        HRole -> (PgAuthId, Nothing)
        HPolicy -> (PgPolicy, Nothing)

    tableName = \case
        PgNamespace -> "pg_namespace"
        PgClass -> "pg_class"
        PgProc -> "pg_proc"
        PgConstraint -> "pg_constraint"
        PgAuthId -> "pg_authid"
        PgLanguage -> "pg_language"
        PgType -> "pg_type"
        PgOperator -> "pg_operator"
        PgAttribute -> "pg_attribute"
        PgTrigger -> "pg_trigger"
        PgAccessMethod -> "pg_am"
        PgCollation -> "pg_collation"
        PgPolicy -> "pg_policy"

    fqObjNameCol = \case
        PgNamespace -> RegularColumn PgNamespace "nspname"
        PgClass -> RegularColumn PgClass "relname"
        PgProc -> RegularColumn PgProc "proname"
        PgConstraint -> RegularColumn PgConstraint "conname"
        PgAuthId -> RegularColumn PgAuthId "rolname"
        PgLanguage -> RegularColumn PgLanguage "lanname"
        PgType -> RegularColumn PgType "typname"
        PgOperator -> RegularColumn PgOperator "oprname"
        PgAttribute -> RegularColumn PgAttribute "attname"
        PgTrigger -> RegularColumn PgTrigger "tgname"
        PgAccessMethod -> RegularColumn PgAccessMethod "amname"
        PgCollation -> RegularColumn PgCollation "collname"
        PgPolicy -> RegularColumn PgPolicy "polname"

    fqTableIdentifyingCols tbl = fqObjNameCol tbl : case tbl of
        PgNamespace -> []
        PgClass -> []
        PgProc -> [OidArrayColumn PgType "proargtypes"]
        PgConstraint -> [OidColumn PgType "contypid"]
        PgAuthId -> []
        PgLanguage -> []
        PgType -> []
        PgOperator -> [OidColumn PgType "oprleft", OidColumn PgType "oprright"]
        PgAttribute -> []
        PgTrigger -> []
        PgAccessMethod -> []
        PgCollation -> [ "collencoding" ]
        PgPolicy -> []

    hashingColsOf = \case
        PgNamespace -> [ OidColumn PgAuthId "nspowner", "nspacl" ]
        PgClass -> [ OidColumn PgType "reltype", OidColumn PgType "reloftype", OidColumn PgAuthId "relowner", OidColumn PgAccessMethod "relam", "relisshared", "relpersistence", "relkind", "relrowsecurity", "relforcerowsecurity", "relreplident", "relispartition", "relacl", "reloptions", "relpartbound" ]
        PgProc -> [ OidColumn PgAuthId "proowner", OidColumn PgLanguage "prolang", OidColumn PgType "provariadic", "prosecdef", "proleakproof", "proisstrict", "proretset", "provolatile", "proparallel", "pronargs", "pronargdefaults", OidColumn PgType "prorettype", OidArrayColumn PgType "proargtypes", OidArrayColumn PgType "proallargtypes", "proargmodes", "proargnames", "proargdefaults", OidArrayColumn PgType "protrftypes", "prosrc", "probin", "proconfig", "proacl" ]
        PgConstraint -> [ "contype", "condeferrable", "condeferred", "convalidated", OidColumn PgClass "conrelid", OidColumn PgType "contypid", OidColumn PgClass "conindid", OidColumn PgClass "confrelid", "confupdtype", "confdeltype", "confmatchtype", "conislocal", "coninhcount", "connoinherit", "conkey", "confkey", OidArrayColumn PgOperator "conpfeqop", OidArrayColumn PgOperator "conppeqop", OidArrayColumn PgOperator "conffeqop", OidArrayColumn PgOperator "conexclop", PureSqlExpression "pg_get_constraintdef(pg_constraint.oid)" ]
        PgAuthId -> [ "rolsuper", "rolinherit", "rolcreaterole", "rolcreatedb", "rolcanlogin", "rolreplication", "rolbypassrls" ]
        PgLanguage -> error "pglanguage cols missing"
        PgType -> error "pgtype cols missing"
        PgOperator -> error "pgoperator cols missing"
        PgAttribute -> [ OidColumn PgType "atttypid", "attnotnull", "atthasdef", PureSqlExpression "(SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND pg_attrdef.adnum=pg_attribute.attnum)"
                        , "attidentity", "attislocal", "attinhcount", OidColumn PgCollation "attcollation", "attacl", "attoptions", "attfdwoptions" ]
        PgTrigger -> [ OidColumn PgProc "tgfoid", "tgtype", "tgenabled", "tgisinternal", OidColumn PgClass "tgconstrrelid", OidColumn PgClass "tgconstrindid", OidColumn PgConstraint "tgconstraint", "tgdeferrable", "tginitdeferred", "tgnargs", "tgattr", "tgargs", "tgqual", "tgoldtable", "tgnewtable" ]
        PgAccessMethod -> error "pg_am cols missing"
        PgCollation -> error "pg_collation cols missing"
        PgPolicy -> [ "polcmd", "polpermissive", OidArrayColumn PgAuthId "polroles", "pg_get_expr(polqual, polrelid)", "pg_get_expr(polwithcheck, polrelid)" ]

    filtersForSchemas includedSchemas = ([], [ ColumnIn (fqObjNameCol PgNamespace) includedSchemas ])
    filtersForRoles includedRoles = ([], [ ColumnIn (fqObjNameCol PgAuthId) includedRoles ])

    underSchemaFilter hobj schemaName = case hobj of
        HTable -> ([ JoinTable "relnamespace" PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName ])
        HView -> ([ JoinTable "relnamespace" PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName ])
        HRoutine -> ([ JoinTable "pronamespace" PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName ])
        HSequence -> ([ JoinTable "relnamespace" PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName ])
        _ -> ([], [])
    
    underTableFilter hobj schemaName tblName = case hobj of
        HColumn -> ([ JoinTable "attrelid" PgClass, JoinTable (RegularColumn PgClass "relnamespace") PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName, ColumnEq (fqObjNameCol PgClass) tblName ])
        HTableConstraint -> ([ JoinTable "conrelid" PgClass, JoinTable (RegularColumn PgClass "relnamespace") PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName, ColumnEq (fqObjNameCol PgClass) tblName ])
        HTrigger -> ([ JoinTable "tgrelid" PgClass, JoinTable (RegularColumn PgClass "relnamespace") PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName, ColumnEq (fqObjNameCol PgClass) tblName ])
        HPolicy -> ([ JoinTable "polrelid" PgClass, JoinTable (RegularColumn PgClass "relnamespace") PgNamespace ], [ ColumnEq (fqObjNameCol PgNamespace) schemaName, ColumnEq (fqObjNameCol PgClass) tblName ] )
        _ -> ([ ], [ ])