module Codd.Hashing.Database.Pg10 (Pg10(..), CatalogTable(..), CatalogTableAliased(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison(..), QueryFrag(..))
import Codd.Hashing.Types (HashableObject(..))
import Debug.Trace (traceShow)

data Pg10 = Pg10
data CatalogTable = PgNamespace | PgClass | PgProc | PgAuthId | PgIndex | PgLanguage | PgType | PgConstraint | PgOperator | PgAttribute | PgTrigger | PgAccessMethod | PgCollation | PgPolicy | PgSequence | PgRoleSettings | PgViews deriving stock Show
data CatalogTableAliased = CatalogTableAliased CatalogTable QueryFrag deriving stock Show

tableAndName :: CatalogTable -> CatalogTableAliased
tableAndName t = CatalogTableAliased t $
    case t of
        PgNamespace -> "pg_namespace"
        PgClass -> "pg_class"
        PgProc -> "pg_proc"
        PgConstraint -> "pg_constraint"
        PgAuthId -> "pg_authid"
        PgIndex -> "pg_index"
        PgLanguage -> "pg_language"
        PgType -> "pg_type"
        PgOperator -> "pg_operator"
        PgAttribute -> "pg_attribute"
        PgTrigger -> "pg_trigger"
        PgAccessMethod -> "pg_am"
        PgCollation -> "pg_collation"
        PgPolicy -> "pg_policy"
        PgSequence -> "pg_sequence"
        PgRoleSettings -> "pg_db_role_setting"
        PgViews -> "pg_views"

instance DbVersionHash Pg10 where
    type CatTable Pg10 = CatalogTableAliased
    hashableObjCatalogTable = \case
        HSchema -> (tableAndName PgNamespace, Nothing)
        HTable -> (tableAndName PgClass, Just "pg_class.relkind IN ('r', 'f', 'p')")
        HView -> (tableAndName PgViews, Nothing)
        HSequence -> (tableAndName PgSequence, Nothing)
        HRoutine -> (tableAndName PgProc, Nothing)
        HColumn -> (tableAndName PgAttribute, Just "NOT pg_attribute.attisdropped AND pg_attribute.attname NOT IN ('cmax', 'cmin', 'ctid', 'tableoid', 'xmax', 'xmin')")
        HIndex -> (tableAndName PgIndex, Nothing)
        HTableConstraint -> (tableAndName PgConstraint, Nothing)
        HTrigger -> (tableAndName PgTrigger, Just "NOT pg_trigger.tgisinternal")
        HRole -> (tableAndName PgAuthId, Nothing)
        HPolicy -> (tableAndName PgPolicy, Nothing)

    tableName (CatalogTableAliased _ alias) = alias

    fqObjNameCol at@(CatalogTableAliased t _) = case t of
        PgNamespace -> RegularColumn at "nspname"
        PgClass -> RegularColumn at "relname"
        PgProc -> RegularColumn at "proname"
        PgConstraint -> RegularColumn at "conname"
        PgAuthId -> RegularColumn at "rolname"
        PgIndex -> RegularColumn at "relname"
        PgLanguage -> RegularColumn at "lanname"
        PgType -> RegularColumn at "typname"
        PgOperator -> RegularColumn at "oprname"
        PgAttribute -> RegularColumn at "attname"
        PgTrigger -> RegularColumn at "tgname"
        PgAccessMethod -> RegularColumn at "amname"
        PgCollation -> RegularColumn at "collname"
        PgPolicy -> RegularColumn at "polname"
        PgSequence -> fqObjNameCol (tableAndName PgClass)
        PgRoleSettings -> error "at shouldn't be querying PgRoleSettings like this!"
        PgViews -> RegularColumn at "viewname"

    fqTableIdentifyingCols at@(CatalogTableAliased t _) = fqObjNameCol at : case t of
        PgNamespace -> []
        PgClass -> []
        PgProc -> [OidArrayColumn (tableAndName PgType) "proargtypes"]
        PgConstraint -> [OidColumn (tableAndName PgType) "contypid"]
        PgAuthId -> []
        PgIndex -> []
        PgLanguage -> []
        PgType -> []
        PgOperator -> [OidColumn (tableAndName PgType) "oprleft", OidColumn (tableAndName PgType) "oprright"]
        PgAttribute -> []
        PgTrigger -> []
        PgAccessMethod -> []
        PgCollation -> [ "collencoding" ]
        PgPolicy -> []
        PgSequence -> []
        PgRoleSettings -> []
        PgViews -> []

    hashingColsOf at@(CatalogTableAliased t _) = traceShow at $ case t of
        PgNamespace -> [ OidColumn (tableAndName PgAuthId) "nspowner", "nspacl" ]
        PgClass -> [ OidColumn (tableAndName PgType) "reltype", OidColumn (tableAndName PgType) "reloftype", OidColumn (tableAndName PgAuthId) "relowner", OidColumn (tableAndName PgAccessMethod) "relam" ] ++ map (RegularColumn at) [ "relisshared", "relpersistence", "relkind", "relrowsecurity", "relforcerowsecurity", "relreplident", "relispartition", "relacl", "reloptions", "relpartbound" ]
        PgProc -> [ OidColumn (tableAndName PgAuthId) "proowner", OidColumn (tableAndName PgLanguage) "prolang", OidColumn (tableAndName PgType) "provariadic", "prosecdef", "proleakproof", "proisstrict", "proretset", "provolatile", "proparallel", "pronargs", "pronargdefaults", OidColumn (tableAndName PgType) "prorettype", OidArrayColumn (tableAndName PgType) "proargtypes", OidArrayColumn (tableAndName PgType) "proallargtypes", "proargmodes", "proargnames", "proargdefaults", OidArrayColumn (tableAndName PgType) "protrftypes", "prosrc", "probin", "proconfig", "proacl" ]
        PgConstraint -> [ "contype", "condeferrable", "condeferred", "convalidated", OidColumn (tableAndName PgClass) "conrelid", OidColumn (tableAndName PgType) "contypid", OidColumn (tableAndName PgClass) "conindid", OidColumn (tableAndName PgClass) "confrelid", "confupdtype", "confdeltype", "confmatchtype", "conislocal", "coninhcount", "connoinherit", "conkey", "confkey", OidArrayColumn (tableAndName PgOperator) "conpfeqop", OidArrayColumn (tableAndName PgOperator) "conppeqop", OidArrayColumn (tableAndName PgOperator) "conffeqop", OidArrayColumn (tableAndName PgOperator) "conexclop", PureSqlExpression "pg_get_constraintdef(pg_constraint.oid)" ]
        PgAuthId -> [ "rolsuper", "rolinherit", "rolcreaterole", "rolcreatedb", "rolcanlogin", "rolreplication", "rolbypassrls", RegularColumn (tableAndName PgRoleSettings) "setconfig" ]
        PgIndex -> [ OidColumn (tableAndName PgClass) "indexrelid", "indisunique", "indisprimary", "indisexclusion", "indimmediate" ] -- TODO: Still missing lots of columns!!
                        ++ hashingColsOf (tableAndName PgClass)
        PgLanguage -> error "pglanguage cols missing"
        PgType -> error "pgtype cols missing"
        PgOperator -> error "pgoperator cols missing"
        PgAttribute -> [ OidColumn (tableAndName PgType) "atttypid", "attnotnull", "atthasdef", PureSqlExpression "(SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND pg_attrdef.adnum=pg_attribute.attnum)"
                        , "attidentity", "attislocal", "attinhcount", OidColumn (tableAndName PgCollation) "attcollation", "attacl", "attoptions", "attfdwoptions" ]
        PgTrigger -> [ OidColumn (tableAndName PgProc) "tgfoid", "tgtype", "tgenabled", "tgisinternal", OidColumn (tableAndName PgClass) "tgconstrrelid", OidColumn (tableAndName PgClass) "tgconstrindid", OidColumn (tableAndName PgConstraint) "tgconstraint", "tgdeferrable", "tginitdeferred", "tgnargs", "tgattr", "tgargs", "tgqual", "tgoldtable", "tgnewtable" ]
        PgAccessMethod -> error "pg_am cols missing"
        PgCollation -> error "pg_collation cols missing"
        PgPolicy -> [ "polcmd", "polpermissive", OidArrayColumn (tableAndName PgAuthId) "polroles", "pg_get_expr(polqual, polrelid)", "pg_get_expr(polwithcheck, polrelid)" ]
        PgSequence -> [ OidColumn (tableAndName PgType) "seqtypid", "seqstart", "seqincrement", "seqmax", "seqmin", "seqcache", "seqcycle" ] ++ hashingColsOf (tableAndName PgClass)
        -- TODO: Owned objects should affect PgClass, not just PgSequence!
        -- Also, maybe it's best that both related objects are affected instead of just a single one? What if, for example, someone changes ownership of
        -- a sequence and someone else renames the column? We want a git conflict in that scenario!
        -- select pg_class.relname, objsubid, refobjid, refobj.relname, refobjsubid, deptype from pg_depend join pg_class on pg_depend.objid=pg_class.oid join pg_class refobj on pg_depend.refobjid=refobj.oid where pg_class.relname='employee_employee_id_seq';

        PgRoleSettings -> []
        PgViews -> hashingColsOf (tableAndName PgClass) ++ map (RegularColumn at) [ "definition" ]

    joinsFor = \case
        HTable -> [ JoinTable "relnamespace" (tableAndName PgNamespace) ]
        HView -> [ JoinTableFull (tableAndName PgNamespace) [(RegularColumn (tableAndName PgViews) "schemaname", RegularColumn (tableAndName PgNamespace) "nspname")], JoinTableFull (tableAndName PgClass) [(RegularColumn (tableAndName PgViews) "viewname", RegularColumn (tableAndName PgClass) "relname"), (RegularColumn (tableAndName PgNamespace) "oid", RegularColumn (tableAndName PgClass) "relnamespace")] ]
        HRoutine -> [ JoinTable "pronamespace" (tableAndName PgNamespace) ]
        HSequence -> [ JoinTable "seqrelid" (tableAndName PgClass), JoinTable "relnamespace" (tableAndName PgNamespace) ]
        HColumn -> [ JoinTable "attrelid" (tableAndName PgClass), JoinTable (RegularColumn (tableAndName PgClass) "relnamespace") (tableAndName PgNamespace) ]
        HIndex -> [ JoinTable "indexrelid" (tableAndName PgClass), JoinTable (RegularColumn (tableAndName PgNamespace) "relnamespace") (tableAndName PgNamespace),
             JoinTable "indrelid" (CatalogTableAliased PgClass "pg_class AS pg_class_idx_table")
             -- ^ A second join to "pg_class AS pg_class_idx_table" for the index's table. This is a nasty way to encode aliases into our internal model.
             ]
        HTableConstraint -> [ JoinTable "conrelid" (tableAndName PgClass), JoinTable (RegularColumn (tableAndName PgClass) "relnamespace") (tableAndName PgNamespace) ]
        HTrigger -> [ JoinTable "tgrelid" (tableAndName PgClass), JoinTable (RegularColumn (tableAndName PgClass) "relnamespace") (tableAndName PgNamespace) ]
        HRole -> [ LeftJoinTable "oid" (tableAndName PgRoleSettings) "setrole" ]
        HPolicy -> [ JoinTable "polrelid" (tableAndName PgClass), JoinTable (RegularColumn (tableAndName PgClass) "relnamespace") (tableAndName PgNamespace) ]
        _ -> []

    filtersForSchemas includedSchemas = [ ColumnIn (fqObjNameCol (tableAndName PgNamespace)) includedSchemas ]
    filtersForRoles includedRoles = [ ColumnIn (fqObjNameCol (tableAndName PgAuthId)) includedRoles ]

    underSchemaFilter hobj schemaName = case hobj of
        HTable -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName ]
        HView -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName ]
        HRoutine -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName ]
        HSequence -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName ]
        _ -> []
    
    underTableFilter hobj schemaName tblName = traceShow ("underTableFilter" ++ show hobj) $ case hobj of
        HColumn -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName, ColumnEq (fqObjNameCol (tableAndName PgClass)) tblName ]
        HTableConstraint -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName, ColumnEq (fqObjNameCol (tableAndName PgClass)) tblName ]
        HTrigger -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName, ColumnEq (fqObjNameCol (tableAndName PgClass)) tblName ]
        HPolicy -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName, ColumnEq (fqObjNameCol (tableAndName PgClass)) tblName ]
        HIndex -> [ ColumnEq (fqObjNameCol (tableAndName PgNamespace)) schemaName, ColumnEq (fqObjNameCol (CatalogTableAliased PgClass "pg_class_idx_table")) tblName ]
        _ -> []