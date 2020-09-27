module Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinFilter(..), CatTable(..), fqcn)
import Codd.Hashing.Types (HashableObject(..))

data Pg10
data CatalogTable = PgNamespace | PgClass | PgProc | PgAuthId | PgLanguage | PgType | PgConstraint | PgOperator | PgAttribute | PgTrigger | PgAccessMethod | PgCollation deriving stock Show

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

    fqObjNameCol = \case
        PgNamespace -> fqcn PgNamespace "nspname"
        PgClass -> fqcn PgClass "relname"
        PgProc -> fqcn PgProc "proname"
        PgConstraint -> fqcn PgConstraint "conname"
        PgAuthId -> fqcn PgAuthId "rolname"
        PgLanguage -> fqcn PgLanguage "lanname"
        PgType -> fqcn PgType "typname"
        PgOperator -> fqcn PgOperator "oprname"
        PgAttribute -> fqcn PgAttribute "attname"
        PgTrigger -> fqcn PgTrigger "tgname"
        PgAccessMethod -> fqcn PgAccessMethod "amname"
        PgCollation -> fqcn PgCollation "collname"

    fqTableIdentifyingCols tbl = fqObjNameCol tbl : case tbl of
        PgNamespace -> []
        PgClass -> []
        PgProc -> [JoinOidArray PgType "proargtypes"]
        PgConstraint -> [JoinOid PgType "contypid"]
        PgAuthId -> []
        PgLanguage -> []
        PgType -> []
        PgOperator -> [JoinOid PgType "oprleft", JoinOid PgType "oprright"]
        PgAttribute -> []
        PgTrigger -> []
        PgAccessMethod -> []
        PgCollation -> [ "collencoding" ]

    hashingColsOf = \case
        PgNamespace -> [ JoinOid PgAuthId "nspowner", "nspacl" ]
        PgClass -> [ JoinOid PgType "reltype", JoinOid PgType "reloftype", JoinOid PgAuthId "relowner", JoinOid PgAccessMethod "relam", "relisshared", "relpersistence", "relkind", "relrowsecurity", "relforcerowsecurity", "relreplident", "relispartition", "relacl", "reloptions", "relpartbound" ]
        PgProc -> [ JoinOid PgAuthId "proowner", JoinOid PgLanguage "prolang", "procost", "prorows", JoinOid PgType "provariadic", "prosecdef", "proleakproof", "proisstrict", "proretset", "provolatile", "proparallel", "pronargs", "pronargdefaults", JoinOid PgType "prorettype", JoinOidArray PgType "proargtypes" ,"proargmodes", "proargnames", "proargdefaults", JoinOidArray PgType "protrftypes", "prosrc", "probin", "proconfig", "proacl" ]
        PgConstraint -> [ "contype", "condeferrable", "condeferred", "convalidated", JoinOid PgClass "conrelid", JoinOid PgType "contypid", JoinOid PgClass "conindid", JoinOid PgClass "confrelid", "confupdtype", "confdeltype", "confmatchtype", "conislocal", "coninhcount", "connoinherit", "conkey", "confkey", JoinOidArray PgOperator "conpfeqop", JoinOidArray PgOperator "conppeqop", JoinOidArray PgOperator "conffeqop", JoinOidArray PgOperator "conexclop", FullyQualifiedColumn "pg_get_constraintdef(pg_constraint.oid)" ]
        PgAuthId -> [ "rolsuper", "rolinherit", "rolcreaterole", "rolcreatedb", "rolcanlogin", "rolreplication", "rolbypassrls" ]
        PgLanguage -> error "pglanguage cols missing"
        PgType -> error "pgtype cols missing"
        PgOperator -> error "pgoperator cols missing"
        PgAttribute -> [ JoinOid PgType "atttypid", "attnotnull", "atthasdef", FullyQualifiedColumn "(SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND pg_attrdef.adnum=pg_attribute.attnum)"
                        , "attidentity", "attislocal", "attinhcount", JoinOid PgCollation "attcollation", "attacl", "attoptions", "attfdwoptions" ]
        PgTrigger -> [ JoinOid PgProc "tgfoid", "tgtype", "tgenabled", "tgisinternal", JoinOid PgClass "tgconstrrelid", JoinOid PgClass "tgconstrindid", JoinOid PgConstraint "tgconstraint", "tgdeferrable", "tginitdeferred", "tgnargs", "tgattr", "tgargs", "tgqual", "tgoldtable", "tgnewtable" ]
        PgAccessMethod -> error "pg_am cols missing"
        PgCollation -> error "pg_collation cols missing"

    underSchemaFilter hobj schemaName = case hobj of
        HTable -> [ JoinFilter "relnamespace" PgNamespace schemaName ]
        HView -> [ JoinFilter "relnamespace" PgNamespace schemaName ]
        HRoutine -> [ JoinFilter "pronamespace" PgNamespace schemaName ]
        HSequence -> [ JoinFilter "relnamespace" PgNamespace schemaName ]
        _ -> [ ]
    
    underTableFilter hobj tblName = case hobj of
        HColumn -> [ JoinFilter "attrelid" PgClass tblName ]
        HTableConstraint -> [ JoinFilter "conrelid" PgClass tblName ]
        HTrigger -> [ JoinFilter "tgrelid" PgClass tblName ]
        _ -> [ ]