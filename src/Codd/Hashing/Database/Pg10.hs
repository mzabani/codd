module Codd.Hashing.Database.Pg10
    ( Pg10(..)
    , CatalogTable(..)
    , hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( CatTable(..)
                                                , CatTableAliased(..)
                                                , CatalogTable(..)
                                                , CatalogTableColumn(..)
                                                , ColumnComparison(..)
                                                , DbVersionHash(..)
                                                , HashQuery(..)
                                                , JoinTable(..)
                                                , QueryFrag(..)
                                                , pgTableName
                                                )
import           Codd.Hashing.Database.SqlGen   ( includeSql )
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( Include
                                                , SqlRole
                                                , SqlSchema
                                                )
import qualified Database.PostgreSQL.Simple    as DB
data Pg10 = Pg10

tableNoAlias :: CatalogTable -> CatTableAliased Pg10
tableNoAlias t = CatTableAliased t (pgTableName t)



aclArrayTbl :: Include SqlRole -> QueryFrag -> QueryFrag
aclArrayTbl allRoles aclArrayIdentifier =
    let acls = "(ACLEXPLODE(" <> aclArrayIdentifier <> "))"
    in
      -- We only include mapped roles for grantees, not for grantors.
      -- Grantee 0 is PUBLIC, which we always want to include.
      -- NOTE: It is not clear what being a grantor means, so we remain open
      -- to having to include 
      -- TODO: Is ARRAY_TO_STRING necessary?
        "(SELECT ARRAY_TO_STRING(ARRAY_AGG(COALESCE(grantee_role.rolname, '') || ';' || privilege_type || ';' || is_grantable ORDER BY grantee_role.rolname, privilege_type, is_grantable), ';') AS permissions_string FROM (SELECT "
        <> acls
        <> ".grantee, "
        <> acls
        <> ".privilege_type, "
        <> acls
        <> ".is_grantable) perms_subq "
        <> "\n LEFT JOIN pg_catalog.pg_roles grantee_role ON grantee_role.oid=perms_subq.grantee "
        <> "\n WHERE grantee_role.rolname IS NULL OR "
        <> includeSql allRoles "grantee_role.rolname"
        <> ")"

oidArrayExpr :: QueryFrag -> QueryFrag -> QueryFrag -> QueryFrag -> QueryFrag
oidArrayExpr oidArrayCol tblToJoin oidColInJoinedTbl hashExpr =
    "(SELECT ARRAY_AGG("
        <> hashExpr
        <> " ORDER BY s._idx)"
        <> "\n FROM UNNEST("
        <> oidArrayCol
        <> ") WITH ORDINALITY s(_oid, _idx)"
        <> "\n JOIN "
        <> tblToJoin
        <> " ON "
        <> tblToJoin
        <> "."
        <> oidColInJoinedTbl
        <> " = s._oid)"

-- | A parenthesized expression of type (oid, op_nspname, op_full_name) whose op_full_name column
-- already includes names of its operators to ensure uniqueness per namespace.
pgOperatorNameTbl :: QueryFrag
pgOperatorNameTbl =
    "(SELECT oid, (oprname || ';' || typleft.typname || '_' || typright.typname || '_' || typret.typname) AS op_full_name"
        <> "\n FROM pg_operator"
        <> "\n JOIN pg_type typleft ON oprleft=typleft.oid"
        <> "\n JOIN pg_type typright ON oprright=typright.oid"
        <> "\n JOIN pg_type typret ON oprresult=typret.oid)"

pgClassHashQuery :: Include SqlRole -> Maybe ObjName -> HashQuery
pgClassHashQuery allRoles schemaName = HashQuery
    { objNameCol    = "pg_class.relname"
    , checksumCols  = [ "pg_reltype.typname"
                      , "pg_reloftype.typname"
                      , "pg_roles.rolname"
                      , "pg_am.amname"
                      , "pg_class.relisshared"
                      , "pg_class.relpersistence"
                      , "pg_class.relkind"
                      , "pg_class.relrowsecurity"
                      , "pg_class.relforcerowsecurity"
                      , "pg_class.relreplident"
                      , "pg_class.relispartition"
                      , "pg_class.reloptions"
                      , "pg_class.relpartbound"
                      , "_codd_roles.permissions_string"
                      ]
    , fromTable     = "pg_catalog.pg_class"
    , joins         =
        "LEFT JOIN pg_catalog.pg_type pg_reltype ON pg_class.reltype=pg_reltype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_type pg_reloftype ON pg_class.reloftype=pg_reloftype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_roles ON pg_class.relowner=pg_roles.oid"
        <> "\nLEFT JOIN pg_catalog.pg_am ON pg_class.relam=pg_am.oid"
        <> "\nLEFT JOIN LATERAL "
        <> aclArrayTbl allRoles "relacl"
        <> " _codd_roles ON TRUE"
        <> "\nJOIN pg_catalog.pg_namespace ON pg_class.relnamespace=pg_namespace.oid"
    , nonIdentWhere = Nothing
    , identWhere    = Just $ maybe ""
                                   (QueryFrag "pg_namespace.nspname=?")
                                   (DB.Only <$> schemaName)
    , groupByCols   = []
    }

hashQueryFor
    :: Include SqlRole
    -> Include SqlSchema
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor allRoles allSchemas schemaName tableName = \case
    HDatabaseSettings -> HashQuery
        { objNameCol    = "datname"
        , checksumCols  = [ "pg_encoding_to_char(encoding)"
                          , "datcollate"
                          , "datctype"
                          ]
        , fromTable     = "pg_catalog.pg_database"
        , joins         = ""
        , nonIdentWhere = Just "datname = current_database()"
        , identWhere    = Nothing
        , groupByCols   = []
        }
    HSchema -> HashQuery
        { objNameCol    = "nspname"
        , checksumCols = ["nsp_owner.rolname", "_codd_roles.permissions_string"]
        , fromTable     = "pg_catalog.pg_namespace"
        , joins         =
            "JOIN pg_catalog.pg_authid AS nsp_owner ON nsp_owner.oid=pg_namespace.nspowner"
            <> "\n LEFT JOIN LATERAL "
            <> aclArrayTbl allRoles "nspacl"
            <> "_codd_roles ON TRUE"
        , nonIdentWhere = Just $ includeSql allSchemas "nspname"
        , identWhere    = Nothing
        , groupByCols   = []
        }
    HRole -> HashQuery
        { objNameCol    = "rolname"
        , checksumCols  = [ "rolsuper"
                          , "rolinherit"
                          , "rolcreaterole"
                          , "rolcreatedb"
                          , "rolcanlogin"
                          , "rolreplication"
                          , "rolbypassrls"
                          , "rolconfig"
                          ]
        , fromTable     = "pg_catalog.pg_roles"
        , joins         = ""
        , nonIdentWhere = Just $ includeSql allRoles "rolname"
        , identWhere    = Nothing
        , groupByCols   = []
        }
    HTable ->
        let hq = pgClassHashQuery allRoles schemaName
        in  hq { nonIdentWhere = Just "relkind IN ('r', 'f', 'p')" }
    HView ->
        let hq = pgClassHashQuery allRoles schemaName
        in
            hq
                { checksumCols = "pg_views.definition" : checksumCols hq
                , joins        =
                    joins hq
                        <> "\nJOIN pg_catalog.pg_views ON pg_views.schemaname=pg_namespace.nspname AND pg_views.viewname=pg_class.relname"
                }
    HPolicy -> HashQuery
        { objNameCol    = "polname"
        , checksumCols  = [ "polcmd"
                          , "polpermissive"
                          , "pg_get_expr(polqual, polrelid)"
                          , "pg_get_expr(polwithcheck, polrelid)"
                          , "ARRAY_AGG(rolname ORDER BY rolname)"
                          ]
        , fromTable     = "pg_catalog.pg_policy"
        , joins         =
            "LEFT JOIN pg_catalog.pg_roles ON pg_roles.oid = ANY(polroles)"
            <> "\nJOIN pg_catalog.pg_class ON pg_class.oid = pg_policy.polrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_class.relnamespace = pg_namespace.oid"
        , nonIdentWhere = Just $ "pg_roles.rolname IS NULL OR " <> includeSql
                              allRoles
                              "pg_roles.rolname"
        , identWhere    = Just
                          $  "TRUE"
                          <> maybe ""
                                   (QueryFrag "\nAND pg_class.relname = ?")
                                   (DB.Only <$> tableName)
                          <> maybe ""
                                   (QueryFrag "\nAND pg_namespace.nspname = ?")
                                   (DB.Only <$> schemaName)
        , groupByCols   = [ "pg_policy.oid"
                          , "polname"
                          , "polcmd"
                          , "polpermissive"
                          , "polqual"
                          , "polrelid"
                          , "polwithcheck"
                          ] -- Need to group by due to join against pg_roles
        }
    HSequence ->
        let hq = pgClassHashQuery allRoles schemaName
        in
            hq
                { checksumCols = checksumCols hq
                                     ++ [ "pg_seq_type.seqtypid"
                                        , "seqstart"
                                        , "seqincrement"
                                        , "seqmax"
                                        , "seqmin"
                                        , "seqcache"
                                        , "seqcycle"
                                        ]
                , joins        =
                    joins hq
                        <> "\nJOIN pg_catalog.pg_sequence pg_seq_type ON seqrelid=pg_class.oid"
                }
    HRoutine ->
        let
            nonAggCols =
                [ "pg_roles.rolname"
                , "pg_language.lanname"
                , "prosecdef"
                , "proleakproof"
                , "proisstrict"
                , "proretset"
                , "provolatile"
                , "proparallel"
                , "pronargs"
                , "pronargdefaults"
                , "pg_type_rettype.typname"
                , "proargmodes"
                , "proargnames"
                , "proargdefaults"
                , "proconfig"
                , "_codd_roles.permissions_string"
                -- The source of the function is important, but "prosrc" is _not_ the source if the function
                -- is compiled, so we ignore this column in those cases.
                -- Note that this means that functions with different implementations could be considered equal,
                -- but I don't know a good way around this
                , "CASE WHEN pg_language.lanispl THEN prosrc END"
                ]
            inputTypesExpr =
                "COALESCE("
                    <> oidArrayExpr "proargtypes" "pg_type" "oid" "typname"
                    <> "::TEXT, '')"
        in
            HashQuery
                { objNameCol    = "proname || ';' || " <> inputTypesExpr
                , checksumCols  = nonAggCols ++ [
                -- TODO: Is '{"", NULL}'::TEXT equal to '{NULL, NULL}'::TEXT ? If so, COALESCE properly to differentiate
                                                 inputTypesExpr]
                , fromTable     = "pg_catalog.pg_proc"
                , joins         =
                    "JOIN pg_catalog.pg_namespace ON pg_namespace.oid=pronamespace"
                    <> "\nJOIN pg_catalog.pg_roles ON pg_roles.oid=proowner"
                    <> "\nLEFT JOIN pg_catalog.pg_language ON pg_language.oid=prolang"
                    <> "\nLEFT JOIN pg_catalog.pg_type pg_type_rettype ON pg_type_rettype.oid=prorettype"
                    <> "\nLEFT JOIN pg_catalog.pg_type pg_type_argtypes ON pg_type_argtypes.oid=ANY(proargtypes)"
                    <> "\n LEFT JOIN LATERAL "
                    <> aclArrayTbl allRoles "proacl"
                    <> "_codd_roles ON TRUE"
                , nonIdentWhere = Nothing
                , identWhere    = Just $ "TRUE" <> maybe
                                      ""
                                      (QueryFrag "\nAND pg_namespace.nspname = ?")
                                      (DB.Only <$> schemaName)
                , groupByCols   = ["proname", "proargtypes"] ++ nonAggCols
                }
    HColumn -> HashQuery
        { objNameCol    = "attname"
        , checksumCols  =
            [ "pg_type.typname"
            , "attnotnull"
            , "atthasdef"
            , "pg_catalog.pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid)"
            , "attidentity"
            , "attislocal"
            , "attinhcount"
            , "pg_collation.collname"
            , "pg_catalog.pg_encoding_to_char(pg_collation.collencoding)"
            , "attoptions"
            , "attfdwoptions"
            , "attnum"
            , "_codd_roles.permissions_string"
            ]
        , fromTable     = "pg_catalog.pg_attribute"
        , joins         =
            "JOIN pg_catalog.pg_class ON pg_class.oid=attrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_namespace.oid=pg_class.relnamespace"
            <> "\nLEFT JOIN pg_catalog.pg_type ON pg_type.oid=atttypid"
            <> "\nLEFT JOIN pg_catalog.pg_attrdef ON pg_attrdef.adrelid=pg_class.oid AND pg_attrdef.adnum=pg_attribute.attnum"
            <> "\nLEFT JOIN pg_collation ON pg_collation.oid=pg_attribute.attcollation"
            <> "\n LEFT JOIN LATERAL "
            <> aclArrayTbl allRoles "attacl"
            <> "_codd_roles ON TRUE"
        , nonIdentWhere =
            Just
                "NOT pg_attribute.attisdropped AND pg_attribute.attname NOT IN ('cmax', 'cmin', 'ctid', 'tableoid', 'xmax', 'xmin')"
        , identWhere    = Just
                          $  "TRUE"
                          <> maybe ""
                                   (QueryFrag "\nAND pg_namespace.nspname = ?")
                                   (DB.Only <$> schemaName)
                          <> maybe ""
                                   (QueryFrag "\nAND pg_class.relname = ?")
                                   (DB.Only <$> tableName)
        , groupByCols   = []
        }
    HTableConstraint -> HashQuery
        { objNameCol = "conname || COALESCE(';' || pg_domain_type.typname, '')"
        , checksumCols  = [ "contype"
                          , "condeferrable"
                          , "condeferred"
                          , "convalidated"
                          , "pg_class_ind.relname"
                          , "pg_class_frel.relname"
                          , "confupdtype"
                          , "confdeltype"
                          , "confmatchtype"
                          , "conislocal"
                          , "coninhcount"
                          , "connoinherit"
                          , "conkey"
                          , "confkey"
            -- , OidArrayColumn (tableNoAlias PgOperator) "conpfeqop"
            -- , OidArrayColumn (tableNoAlias PgOperator) "conppeqop"
            -- , OidArrayColumn (tableNoAlias PgOperator) "conffeqop"
            -- , OidArrayColumn (tableNoAlias PgOperator) "conexclop"
                          , "pg_get_constraintdef(pg_constraint.oid)"
                          ]
        , fromTable     = "pg_catalog.pg_constraint"
        , joins         =
            "JOIN pg_catalog.pg_class ON pg_class.oid=conrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_namespace.oid=pg_class.relnamespace"
            <> "\nLEFT JOIN pg_catalog.pg_type pg_domain_type ON pg_domain_type.oid=pg_constraint.contypid"
            <> "\nLEFT JOIN pg_catalog.pg_class pg_class_ind ON pg_class_ind.oid=pg_constraint.conindid"
            <> "\nLEFT JOIN pg_catalog.pg_class pg_class_frel ON pg_class_frel.oid=pg_constraint.confrelid"
                -- <> "\nLEFT JOIN LATERAL " <> oidArrayTbl "conpfeqop" "pg_operator" "oid" "oprname" <> " pfeqop ON TRUE"
                -- <> "\nLEFT JOIN LATERAL " <> oidArrayTbl "conpfeqop" "pg_operator" "oid" "oprname" <> " pfeqop ON TRUE"
                -- <> "\nLEFT JOIN LATERAL " <> oidArrayTbl "conpfeqop" "pg_operator" "oid" "oprname" <> " pfeqop ON TRUE"
                -- <> "\nLEFT JOIN LATERAL " <> oidArrayTbl "conpfeqop" "pg_operator" "oid" "oprname" <> " pfeqop ON TRUE"
        , nonIdentWhere = Nothing
        , identWhere    = Just
                          $  "TRUE"
                          <> maybe ""
                                   (QueryFrag "\nAND pg_namespace.nspname = ?")
                                   (DB.Only <$> schemaName)
                          <> maybe ""
                                   (QueryFrag "\nAND pg_class.relname = ?")
                                   (DB.Only <$> tableName)
        , groupByCols   = []
        }
    HIndex ->
        let hq = pgClassHashQuery allRoles schemaName
        in
            hq
                {
            -- TODO: Lots of columns still missing!!
                  checksumCols = checksumCols hq
                                     ++ [ "indisunique"
                                        , "indisprimary"
                                        , "indisexclusion"
                                        , "indimmediate"
                                        ]
                , joins        =
                    joins hq
                        <> "\n JOIN pg_catalog.pg_index ON pg_index.indexrelid=pg_class.oid \
                                  \\n JOIN pg_catalog.pg_class pg_index_table ON pg_index.indrelid=pg_index_table.oid"
                , identWhere   =
                    (<> maybe ""
                              (QueryFrag " AND pg_index_table.relname=?")
                              (DB.Only <$> tableName)
                        )
                        <$> identWhere hq
                }
    HTrigger -> HashQuery
        { objNameCol    = "tgname"
        , checksumCols  = [ "pg_proc.proname" -- TODO: Use full function name that includes types!!
                          , "tgtype"
                          , "tgenabled"
                          , "tgisinternal"
                          , "pg_ref_table.relname"
                          , "pg_trigger_ind.relname"
                          , "pg_trigger_constr.conname" -- TODO: Use full constraint name that includes domain type!!
                          , "tgdeferrable"
                          , "tginitdeferred"
                          , "tgnargs"
                          , "tgattr"
                          , "tgargs"
                          , "tgqual"
                          , "tgoldtable"
                          , "tgnewtable"
                          ]
        , fromTable     = "pg_catalog.pg_trigger"
        , joins         =
            "JOIN pg_catalog.pg_class pg_trigger_table ON pg_trigger_table.oid=pg_trigger.tgrelid \
               \\n JOIN pg_catalog.pg_namespace ON pg_namespace.oid=pg_trigger_table.relnamespace \
               \\n LEFT JOIN pg_catalog.pg_proc ON pg_proc.oid=tgfoid \
               \\n LEFT JOIN pg_catalog.pg_class pg_ref_table ON pg_ref_table.oid=tgconstrrelid \
               \\n LEFT JOIN pg_catalog.pg_class pg_trigger_ind ON pg_trigger_ind.oid=tgconstrindid \
               \\n LEFT JOIN pg_catalog.pg_constraint pg_trigger_constr ON pg_trigger_constr.oid=tgconstraint"
        , nonIdentWhere = Just "NOT tgisinternal"
        , identWhere    = Just
                          $  "TRUE"
                          <> maybe ""
                                   (QueryFrag "\nAND pg_namespace.nspname = ?")
                                   (DB.Only <$> schemaName)
                          <> maybe
                                 ""
                                 (QueryFrag "\nAND pg_trigger_table.relname = ?")
                                 (DB.Only <$> tableName)
        , groupByCols   = []
        }

    _ -> error "not implemented"

instance DbVersionHash Pg10 where
    type CatTable Pg10 = CatalogTable
    hashableObjCatalogTable = \case
        HDatabaseSettings ->
            ( tableNoAlias PgDatabase
            , Just "pg_database.datname = current_database()"
            )
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
        PgDatabase     -> "pg_database"
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
        PgDatabase     -> RegularColumn at "datname"
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
            PgDatabase   -> []
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
        PgDatabase -> PureSqlExpression "pg_encoding_to_char(encoding)"
            : map (RegularColumn at) ["datcollate", "datctype"]
        PgNamespace ->
            [ OidColumn (tableNoAlias PgAuthId) "nspowner"
            , AclItemsColumn at "nspacl"
            ]
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
                    ++ [AclItemsColumn at "relacl"]
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
            , AclItemsColumn at "proacl"
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
            , AclItemsColumn at "attacl"
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
                                                                                                                                                                                            -- A second join to "pg_class AS pg_class_idx_table" for the index's table. This is a nasty way to encode aliases into our internal model.
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
