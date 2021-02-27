module Codd.Hashing.Database.Pg10
    ( Pg10(..)
    , CatalogTable(..)
    ) where

import           Codd.Hashing.Database.Model    ( CatTable(..)
                                                , CatTableAliased(..)
                                                , CatalogTable(..)
                                                , CatalogTableColumn(..)
                                                , ColumnComparison(..)
                                                , DbVersionHash(..)
                                                , JoinTable(..)
                                                , pgTableName
                                                )
import           Codd.Hashing.Types             ( HashableObject(..) )

data Pg10 = Pg10

tableNoAlias :: CatalogTable -> CatTableAliased Pg10
tableNoAlias t = CatTableAliased t (pgTableName t)

instance DbVersionHash Pg10 where
    type CatTable Pg10 = CatalogTable
    hashableObjCatalogTable = \case
        HSchema -> (tableNoAlias PgNamespace, Nothing)
        HTable ->
            (tableNoAlias PgClass, Just "pg_class.relkind IN ('r', 'f', 'p')")
        HView     -> (tableNoAlias PgViews, Nothing)
        HSequence -> (tableNoAlias PgSequence, Nothing)
        HRoutine  -> (tableNoAlias PgProc, Nothing)
        HColumn ->
            ( tableNoAlias PgAttribute
            , Just
                "NOT pg_attribute.attisdropped AND pg_attribute.attname NOT IN ('cmax', 'cmin', 'ctid', 'tableoid', 'xmax', 'xmin')"
            )
        HIndex           -> (tableNoAlias PgIndex, Nothing)
        HTableConstraint -> (tableNoAlias PgConstraint, Nothing)
        HTrigger ->
            (tableNoAlias PgTrigger, Just "NOT pg_trigger.tgisinternal")
        HRole   -> (tableNoAlias PgAuthId, Nothing)
        HPolicy -> (tableNoAlias PgPolicy, Nothing)

    tableName (CatTableAliased tbl _) = case tbl of
        PgNamespace    -> "pg_namespace"
        PgClass        -> "pg_class"
        PgProc         -> "pg_proc"
        PgConstraint   -> "pg_constraint"
        PgAuthId       -> "pg_authid"
        PgIndex        -> "pg_index"
        PgLanguage     -> "pg_language"
        PgType         -> "pg_type"
        PgOperator     -> "pg_operator"
        PgAttribute    -> "pg_attribute"
        PgTrigger      -> "pg_trigger"
        PgAccessMethod -> "pg_am"
        PgCollation    -> "pg_collation"
        PgPolicy       -> "pg_policy"
        PgSequence     -> "pg_sequence"
        PgRoleSettings -> "pg_db_role_setting"
        PgViews        -> "pg_views"

    fqObjNameCol at@(CatTableAliased t _) = case t of
        PgNamespace    -> RegularColumn at "nspname"
        PgClass        -> RegularColumn at "relname"
        PgProc         -> RegularColumn at "proname"
        PgConstraint   -> RegularColumn at "conname"
        PgAuthId       -> RegularColumn at "rolname"
        PgIndex        -> RegularColumn (tableNoAlias PgClass) "relname"
        PgLanguage     -> RegularColumn at "lanname"
        PgType         -> RegularColumn at "typname"
        PgOperator     -> RegularColumn at "oprname"
        PgAttribute    -> RegularColumn at "attname"
        PgTrigger      -> RegularColumn at "tgname"
        PgAccessMethod -> RegularColumn at "amname"
        PgCollation    -> RegularColumn at "collname"
        PgPolicy       -> RegularColumn at "polname"
        PgSequence     -> fqObjNameCol (tableNoAlias PgClass)
        PgRoleSettings ->
            error "at shouldn't be querying PgRoleSettings like this!"
        PgViews -> RegularColumn at "viewname"

    fqTableIdentifyingCols at@(CatTableAliased t _) =
        fqObjNameCol at : case t of
            PgNamespace  -> []
            PgClass      -> []
            PgProc       -> [OidArrayColumn (tableNoAlias PgType) "proargtypes"]
            PgConstraint -> [OidColumn (tableNoAlias PgType) "contypid"]
            PgAuthId     -> []
            PgIndex      -> []
            PgLanguage   -> []
            PgType       -> []
            PgOperator ->
                [ OidColumn (tableNoAlias PgType) "oprleft"
                , OidColumn (tableNoAlias PgType) "oprright"
                ]
            PgAttribute    -> []
            PgTrigger      -> []
            PgAccessMethod -> []
            PgCollation    -> ["collencoding"]
            PgPolicy       -> []
            PgSequence     -> []
            PgRoleSettings -> []
            PgViews        -> []

    hashingColsOf at@(CatTableAliased t _) = case t of
        PgNamespace -> [OidColumn (tableNoAlias PgAuthId) "nspowner", "nspacl"]
        PgClass ->
            let oidCols =
                    [ (PgType        , "reltype")
                    , (PgType        , "reloftype")
                    , (PgAuthId      , "relowner")
                    , (PgAccessMethod, "relam")
                    ]
                otherCols =
                    [ "relisshared"
                    , "relpersistence"
                    , "relkind"
                    , "relrowsecurity"
                    , "relforcerowsecurity"
                    , "relreplident"
                    , "relispartition"
                    , "relacl"
                    , "reloptions"
                    , "relpartbound"
                    ]
            in  map
                        (\(tbl, col) -> OidColumn
                            (tableNoAlias tbl)
                            (RegularColumn (tableNoAlias PgClass) col)
                        )
                        oidCols
                    ++ map (RegularColumn at) otherCols
        PgProc ->
            [ OidColumn (tableNoAlias PgAuthId)   "proowner"
            , OidColumn (tableNoAlias PgLanguage) "prolang"
            , OidColumn (tableNoAlias PgType)     "provariadic"
            , "prosecdef"
            , "proleakproof"
            , "proisstrict"
            , "proretset"
            , "provolatile"
            , "proparallel"
            , "pronargs"
            , "pronargdefaults"
            , OidColumn (tableNoAlias PgType) "prorettype"
            , OidArrayColumn (tableNoAlias PgType) "proargtypes"
            , OidArrayColumn (tableNoAlias PgType) "proallargtypes"
            , "proargmodes"
            , "proargnames"
            , "proargdefaults"
            , OidArrayColumn (tableNoAlias PgType) "protrftypes"
            , "prosrc"
            , "probin"
            , "proconfig"
            , "proacl"
            ]
        PgConstraint ->
            [ "contype"
            , "condeferrable"
            , "condeferred"
            , "convalidated"
            , OidColumn (tableNoAlias PgClass) "conrelid"
            , OidColumn (tableNoAlias PgType)  "contypid"
            , OidColumn (tableNoAlias PgClass) "conindid"
            , OidColumn (tableNoAlias PgClass) "confrelid"
            , "confupdtype"
            , "confdeltype"
            , "confmatchtype"
            , "conislocal"
            , "coninhcount"
            , "connoinherit"
            , "conkey"
            , "confkey"
            , OidArrayColumn (tableNoAlias PgOperator) "conpfeqop"
            , OidArrayColumn (tableNoAlias PgOperator) "conppeqop"
            , OidArrayColumn (tableNoAlias PgOperator) "conffeqop"
            , OidArrayColumn (tableNoAlias PgOperator) "conexclop"
            , PureSqlExpression "pg_get_constraintdef(pg_constraint.oid)"
            ]
        PgAuthId ->
            [ "rolsuper"
            , "rolinherit"
            , "rolcreaterole"
            , "rolcreatedb"
            , "rolcanlogin"
            , "rolreplication"
            , "rolbypassrls"
            , RegularColumn (tableNoAlias PgRoleSettings) "setconfig"
            ]
        PgIndex ->
            ["indisunique", "indisprimary", "indisexclusion", "indimmediate"] -- TODO: Still missing lots of columns!!
                ++ hashingColsOf (tableNoAlias PgClass)
        PgLanguage -> error "pglanguage cols missing"
        PgType     -> error "pgtype cols missing"
        PgOperator -> error "pgoperator cols missing"
        PgAttribute ->
            [ OidColumn (tableNoAlias PgType) "atttypid"
            , "attnotnull"
            , "atthasdef"
            , PureSqlExpression
                "(SELECT pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid) FROM pg_catalog.pg_attrdef WHERE pg_attrdef.adrelid=pg_attribute.attrelid AND pg_attrdef.adnum=pg_attribute.attnum)"
            , "attidentity"
            , "attislocal"
            , "attinhcount"
            , OidColumn (tableNoAlias PgCollation) "attcollation"
            , "attacl"
            , "attoptions"
            , "attfdwoptions"
            , "attnum"
            ]
        PgTrigger ->
            [ OidColumn (tableNoAlias PgProc) "tgfoid"
            , "tgtype"
            , "tgenabled"
            , "tgisinternal"
            , OidColumn (tableNoAlias PgClass)      "tgconstrrelid"
            , OidColumn (tableNoAlias PgClass)      "tgconstrindid"
            , OidColumn (tableNoAlias PgConstraint) "tgconstraint"
            , "tgdeferrable"
            , "tginitdeferred"
            , "tgnargs"
            , "tgattr"
            , "tgargs"
            , "tgqual"
            , "tgoldtable"
            , "tgnewtable"
            ]
        PgAccessMethod -> error "pg_am cols missing"
        PgCollation    -> error "pg_collation cols missing"
        PgPolicy ->
            [ "polcmd"
            , "polpermissive"
            , OidArrayColumn (tableNoAlias PgAuthId) "polroles"
            , "pg_get_expr(polqual, polrelid)"
            , "pg_get_expr(polwithcheck, polrelid)"
            ]
        PgSequence ->
            [ OidColumn (tableNoAlias PgType) "seqtypid"
                , "seqstart"
                , "seqincrement"
                , "seqmax"
                , "seqmin"
                , "seqcache"
                , "seqcycle"
                ]
                ++ hashingColsOf (tableNoAlias PgClass)
        -- TODO: Owned objects should affect PgClass, not just PgSequence!
        -- Also, maybe it's best that both related objects are affected instead of just a single one? What if, for example, someone changes ownership of
        -- a sequence and someone else renames the column? We want a git conflict in that scenario!
        -- select pg_class.relname, objsubid, refobjid, refobj.relname, refobjsubid, deptype from pg_depend join pg_class on pg_depend.objid=pg_class.oid join pg_class refobj on pg_depend.refobjid=refobj.oid where pg_class.relname='employee_employee_id_seq';

        PgRoleSettings -> []
        PgViews        -> hashingColsOf (tableNoAlias PgClass)
            ++ map (RegularColumn at) ["definition"]

    joinsFor = \case
        HTable -> [JoinTable "relnamespace" (tableNoAlias PgNamespace)]
        HView ->
            [ JoinTableFull
                (tableNoAlias PgNamespace)
                [ ( RegularColumn (tableNoAlias PgViews)     "schemaname"
                  , RegularColumn (tableNoAlias PgNamespace) "nspname"
                  )
                ]
            , JoinTableFull
                (tableNoAlias PgClass)
                [ ( RegularColumn (tableNoAlias PgViews) "viewname"
                  , RegularColumn (tableNoAlias PgClass) "relname"
                  )
                , ( RegularColumn (tableNoAlias PgNamespace) "oid"
                  , RegularColumn (tableNoAlias PgClass)     "relnamespace"
                  )
                ]
            ]
        HRoutine -> [JoinTable "pronamespace" (tableNoAlias PgNamespace)]
        HSequence ->
            [ JoinTable "seqrelid"     (tableNoAlias PgClass)
            , JoinTable "relnamespace" (tableNoAlias PgNamespace)
            ]
        HColumn ->
            [ JoinTable "attrelid" (tableNoAlias PgClass)
            , JoinTable
                (RegularColumn (tableNoAlias PgClass) "relnamespace")
                (tableNoAlias PgNamespace)
            ]
        HIndex ->
            let pgClassAliased = CatTableAliased PgClass "pg_class_idx_table"
            in  [ JoinTable "indexrelid" (tableNoAlias PgClass)
                , JoinTable "indrelid"   pgClassAliased
                    -- ^ A second join to "pg_class AS pg_class_idx_table" for the index's table. This is a nasty way to encode aliases into our internal model.
                , JoinTableFull
                    (tableNoAlias PgNamespace)
                    [ ( RegularColumn pgClassAliased             "relnamespace"
                      , RegularColumn (tableNoAlias PgNamespace) "oid"
                      )
                    ]
                ]
        HTableConstraint ->
            [ JoinTable "conrelid" (tableNoAlias PgClass)
            , JoinTable
                (RegularColumn (tableNoAlias PgClass) "relnamespace")
                (tableNoAlias PgNamespace)
            ]
        HTrigger ->
            [ JoinTable "tgrelid" (tableNoAlias PgClass)
            , JoinTable
                (RegularColumn (tableNoAlias PgClass) "relnamespace")
                (tableNoAlias PgNamespace)
            ]
        HRole -> [LeftJoinTable "oid" (tableNoAlias PgRoleSettings) "setrole"]
        HPolicy ->
            [ JoinTable "polrelid" (tableNoAlias PgClass)
            , JoinTable
                (RegularColumn (tableNoAlias PgClass) "relnamespace")
                (tableNoAlias PgNamespace)
            ]
        _ -> []

    filtersForSchemas includedSchemas =
        [ColumnIn (fqObjNameCol (tableNoAlias PgNamespace)) includedSchemas]
    filtersForRoles includedRoles =
        [ColumnIn (fqObjNameCol (tableNoAlias PgAuthId)) includedRoles]

    underSchemaFilter hobj schemaName = case hobj of
        HTable ->
            [ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName]
        HView ->
            [ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName]
        HRoutine ->
            [ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName]
        HSequence ->
            [ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName]
        _ -> []

    underTableFilter hobj schemaName tblName = case hobj of
        HColumn ->
            [ ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName
            , ColumnEq (fqObjNameCol (tableNoAlias PgClass))     tblName
            ]
        HTableConstraint ->
            [ ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName
            , ColumnEq (fqObjNameCol (tableNoAlias PgClass))     tblName
            ]
        HTrigger ->
            [ ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName
            , ColumnEq (fqObjNameCol (tableNoAlias PgClass))     tblName
            ]
        HPolicy ->
            [ ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName
            , ColumnEq (fqObjNameCol (tableNoAlias PgClass))     tblName
            ]
        HIndex ->
            [ ColumnEq (fqObjNameCol (tableNoAlias PgNamespace)) schemaName
            , ColumnEq
                (fqObjNameCol (CatTableAliased PgClass "pg_class_idx_table"))
                tblName
            ]
        _ -> []
