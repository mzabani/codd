module Codd.Hashing.Database.Pg10
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..)
                                                , QueryFrag(..)
                                                )
import           Codd.Hashing.Database.SqlGen   ( includeSql
                                                , safeStringConcat
                                                )
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( Include
                                                , SqlRole
                                                , SqlSchema
                                                )
import qualified Database.PostgreSQL.Simple    as DB

aclArrayTbl :: Include SqlRole -> QueryFrag -> QueryFrag
aclArrayTbl allRoles aclArrayIdentifier =
    let acls = "(ACLEXPLODE(" <> aclArrayIdentifier <> "))"
    in
      -- We only include mapped roles for grantees, not for grantors.
      -- Grantee 0 is PUBLIC, which we always want to include.
      -- NOTE: It is not clear what being a grantor means, so we remain open
      -- to having to include 
        "(SELECT ARRAY_AGG(COALESCE(grantee_role.rolname, '') || ';' || privilege_type || ';' || is_grantable ORDER BY grantee_role.rolname, privilege_type, is_grantable) AS permissions FROM (SELECT "
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

-- | Receives a SQL expression that is an array and returns a SQL expression that is that array, sorted.
sortArrayExpr :: QueryFrag -> QueryFrag
sortArrayExpr col =
    "(select array_agg(x.* order by x.*) from unnest(" <> col <> ") x)"

-- | Given the alias for a in-context "pg_proc" table, returns an expression
--   of type text with the function's unambiguous name - one that includes
--   the names of its input arguments' types.
pronameExpr :: QueryFrag -> QueryFrag
pronameExpr pronameTbl =
    qual "proname || ';' || "
        <> "ARRAY_TO_STRING(COALESCE("
        <> oidArrayExpr (qual "proargtypes")
                        "pg_catalog.pg_type"
                        "oid"
                        "typname"
        <> ", '{}'), ',')"
    where qual e = pronameTbl <> "." <> e

-- | Given aliases for in-context "pg_proc" and "pg_domain_type" tables, returns an expression
--   of type text with the constraints's unambiguous name - one that includes
--   the name of its type.
constraintnameExpr :: QueryFrag -> QueryFrag -> QueryFrag
constraintnameExpr conTbl pgDomainTypeTbl =
    conTbl
        <> ".conname || COALESCE(';' || "
        <> pgDomainTypeTbl
        <> ".typname, '')"

-- | A parenthesized expression of type (oid, op_nspname, op_full_name) whose op_full_name column
-- already includes names of its operators to ensure uniqueness per namespace.
-- We still don't use it, but it might become useful in the future.
_pgOperatorNameTbl :: QueryFrag
_pgOperatorNameTbl =
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
                      , "rel_owner_role.rolname"
                      , "pg_am.amname"
                      , "pg_class.relisshared"
                      , "pg_class.relpersistence"
                      , "pg_class.relkind"
                      , "pg_class.relrowsecurity"
                      , "pg_class.relforcerowsecurity"
                      , "pg_class.relreplident"
                      , "pg_class.relispartition"
                      , sortArrayExpr "pg_class.reloptions"
                      -- , "pg_class.relpartbound" -- a pg_node_tree for partition bound, but I couldn't find a function to get its definition
                      , "_codd_roles.permissions"
                      ]
    , fromTable     = "pg_catalog.pg_class"
    , joins         =
        "LEFT JOIN pg_catalog.pg_type pg_reltype ON pg_class.reltype=pg_reltype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_type pg_reloftype ON pg_class.reloftype=pg_reloftype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_roles rel_owner_role ON pg_class.relowner=rel_owner_role.oid"
        <> "\nLEFT JOIN pg_catalog.pg_am ON pg_class.relam=pg_am.oid"
        <> "\nLEFT JOIN LATERAL "
        <> aclArrayTbl allRoles "pg_class.relacl"
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
    HDatabaseSettings ->
        let nonAggCols =
                ["pg_encoding_to_char(encoding)", "datcollate", "datctype"]
        in
            HashQuery
                { objNameCol    = "datname"
                , checksumCols  = [ "pg_encoding_to_char(encoding)"
                                  , "datcollate"
                                  , "datctype"
                                  , sortArrayExpr
                                  $  "ARRAY_AGG("
                                  <> safeStringConcat
                                         [ "pg_settings.name"
                                         , "pg_settings.setting"
                                         -- , "pg_settings.reset_val" -- Sadly, reset_val is only reflected by `ALTER DATABASE SET ..` in newly opened connections
                                         , "pg_settings.min_val"
                                         , "pg_settings.max_val"
                                         , sortArrayExpr "pg_settings.enumvals"
                                         ]
                                  <> " ORDER BY pg_settings.name)"
                                  ]
                , fromTable     = "pg_catalog.pg_database"
                , joins         = "LEFT JOIN pg_catalog.pg_settings ON TRUE" -- pg_settings assumes values from the current database
                , nonIdentWhere =
                    Just
                        "datname = current_database() AND (pg_settings.name IS NULL OR pg_settings.name IN ('default_transaction_isolation', 'default_transaction_deferrable', 'default_transaction_read_only'))" -- TODO: What other settings matter for correctness?
                , identWhere    = Nothing
                , groupByCols   = "datname" : nonAggCols
                }
    HSchema -> HashQuery
        { objNameCol    = "nspname"
        , checksumCols  = ["nsp_owner.rolname", "_codd_roles.permissions"]
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
    HRole ->
        let nonAggCols =
                [ "pg_roles.rolsuper"
                , "pg_roles.rolinherit"
                , "pg_roles.rolcreaterole"
                , "pg_roles.rolcreatedb"
                , "pg_roles.rolcanlogin"
                , "pg_roles.rolreplication"
                , "pg_roles.rolbypassrls"
                , sortArrayExpr "pg_roles.rolconfig"
                , "_codd_roles.permissions"
                ]
        in
            HashQuery
                { objNameCol    = "pg_roles.rolname"
                , checksumCols  =
                    nonAggCols
                        ++ [ "ARRAY_AGG(other_role.rolname || ';' || pg_auth_members.admin_option ORDER BY other_role.rolname, pg_auth_members.admin_option)"
                           ]
                , fromTable     = "pg_catalog.pg_roles"
                , joins         =
                    "JOIN pg_catalog.pg_database ON pg_database.datname = current_database() \
         \\n LEFT JOIN pg_catalog.pg_auth_members ON pg_auth_members.member=pg_roles.oid \
         \\n LEFT JOIN pg_catalog.pg_roles other_role ON other_role.oid=pg_auth_members.roleid \
         \\n LEFT JOIN LATERAL "
                    <> dbPermsTable
                    <> " _codd_roles ON TRUE"
                , nonIdentWhere = Just $ includeSql allRoles "pg_roles.rolname"
                , identWhere    = Nothing
                , groupByCols   = "pg_roles.rolname" : nonAggCols
                }
      where
        dbPermsTable :: QueryFrag
        dbPermsTable =
            let acls = "(ACLEXPLODE(pg_database.datacl))"
            in
                "(SELECT ARRAY_AGG(COALESCE(grantee_role.rolname, '') || ';' || privilege_type || ';' || is_grantable ORDER BY grantee_role.rolname, privilege_type, is_grantable) AS permissions FROM (SELECT "
                <> acls
                <> ".grantee, "
                <> acls
                <> ".privilege_type, "
                <> acls
                <> ".is_grantable) perms_subq "
                <> "\n INNER JOIN pg_catalog.pg_roles grantee_role ON grantee_role.oid=perms_subq.grantee "
                <> "\n WHERE grantee_role.rolname=pg_roles.rolname"
                <> ")"
    HTable ->
        let hq = pgClassHashQuery allRoles schemaName
        in  hq { nonIdentWhere = Just "relkind IN ('r', 'f', 'p')" }
    HView ->
        let hq = pgClassHashQuery allRoles schemaName
        in
            hq
                { checksumCols = "pg_views.definition"
                                     : [ "pg_reltype.typname"
                                       , "pg_reloftype.typname"
                                       , "rel_owner_role.rolname"
                                       , "pg_am.amname"
                                       , "pg_class.relisshared"
                                       , "pg_class.relpersistence"
                                       , "pg_class.relkind"
                                       , "pg_class.relrowsecurity"
                                       , "pg_class.relforcerowsecurity"
                                       , "pg_class.relreplident"
                                       , "pg_class.relispartition"
                                       , sortArrayExpr "pg_class.reloptions"
                                       , "pg_class.relpartbound"
                                       , "_codd_roles.permissions"
                                       ]
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
                                     ++ [ "pg_seq_type.typname"
                                        , "seqstart"
                                        , "seqincrement"
                                        , "seqmax"
                                        , "seqmin"
                                        , "seqcache"
                                        , "seqcycle"
                                        , "owner_column.tablename"
                                        , "owner_column.colnum" -- Num instead of name doesn't touch the sequence if the column's renamed,
                                                                -- but touches it if the column changes positions (which is probably better)
                                        ]
                , joins        =
                    joins hq
                        <> "\nJOIN pg_catalog.pg_sequence pg_sequence ON seqrelid=pg_class.oid \
                          \\n JOIN pg_catalog.pg_type AS pg_seq_type ON pg_seq_type.oid=pg_sequence.seqtypid \
                          \\n LEFT JOIN (SELECT pg_depend.objid AS sequence_oid, owner_Col_table.relname AS tablename, pg_attribute.attnum AS colnum \
                          \\n      FROM pg_catalog.pg_depend \
                          \\n         JOIN pg_catalog.pg_attribute ON pg_attribute.attrelid=pg_depend.refobjid AND pg_attribute.attnum=pg_depend.refobjsubid \
                          \\n         JOIN pg_catalog.pg_class owner_col_table ON owner_col_table.oid=pg_attribute.attrelid) owner_column \
                          \\n            ON owner_column.sequence_oid=pg_class.oid"
                }
    HRoutine ->
        let nonAggCols =
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
                -- , "proargdefaults" -- pg_node_tree type (avoid)
                , "pg_catalog.pg_get_function_arguments(pg_proc.oid)"
                , sortArrayExpr "proconfig" -- Not sure what this is, but let's be conservative and sort it meanwhile
                , "_codd_roles.permissions"
                -- The source of the function is important, but "prosrc" is _not_ the source if the function
                -- is compiled, so we ignore this column in those cases.
                -- Note that this means that functions with different implementations could be considered equal,
                -- but I don't know a good way around this
                , "CASE WHEN pg_language.lanispl THEN prosrc END"
                ]
        in
            HashQuery
                { objNameCol    = pronameExpr "pg_proc"
                , checksumCols  = nonAggCols
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
            , sortArrayExpr "attoptions"
            , sortArrayExpr "attfdwoptions"
            , "attnum"
            , "_codd_roles.permissions"
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
        { objNameCol    = constraintnameExpr "pg_constraint" "pg_domain_type"
        , checksumCols  = [ "pg_constraint.contype"
                          , "pg_constraint.condeferrable"
                          , "pg_constraint.condeferred"
                          , "pg_constraint.convalidated"
                          , "pg_class_ind.relname"
                          , "pg_class_frel.relname"
                          , "pg_constraint.confupdtype"
                          , "pg_constraint.confdeltype"
                          , "pg_constraint.confmatchtype"
                          , "pg_constraint.conislocal"
                          , "pg_constraint.coninhcount"
                          , "pg_constraint.connoinherit"
                          , "pg_constraint.conkey"
                          , "pg_constraint.confkey"
                          , "pg_get_constraintdef(pg_constraint.oid)"
                          -- , "conbin" -- A pg_node_tree
                          ]
        , fromTable     = "pg_catalog.pg_constraint"
        , joins         =
            "JOIN pg_catalog.pg_class ON pg_class.oid=pg_constraint.conrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_namespace.oid=pg_class.relnamespace"
            <> "\nLEFT JOIN pg_catalog.pg_type pg_domain_type ON pg_domain_type.oid=pg_constraint.contypid"
            <> "\nLEFT JOIN pg_catalog.pg_class pg_class_ind ON pg_class_ind.oid=pg_constraint.conindid"
            <> "\nLEFT JOIN pg_catalog.pg_class pg_class_frel ON pg_class_frel.oid=pg_constraint.confrelid"
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
            -- TODO: Lots of columns still missing!! But pg_get_indexdef should do a good enough job for now
                  checksumCols = checksumCols hq
                                     ++ [ "indisunique"
                                        , "indisprimary"
                                        , "indisexclusion"
                                        , "indimmediate"
                                        , "pg_get_indexdef(pg_index.indexrelid)"
                                        -- , "indexprs" -- pg_node_tree
                                        -- , "indpred" -- pg_node_tree
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
        , checksumCols  = [ pronameExpr "pg_proc"
                          , "tgtype"
                          , "tgenabled"
                          , "tgisinternal"
                          , "pg_ref_table.relname"
                          , "pg_trigger_ind.relname"
                          , constraintnameExpr "pg_trigger_constr"
                                               "pg_trigger_constr_type"
                          , "tgdeferrable"
                          , "tginitdeferred"
                          , "tgnargs"
                          , "tgattr"
                          , "tgargs"
                          -- , "tgqual" -- This is system dependent. Equal expression can have different pg_node_tree::text representations
                          -- With the inclusion below, many other columns are probably unnecessary
                          , "pg_catalog.pg_get_triggerdef(pg_trigger.oid)"
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
               \\n LEFT JOIN pg_catalog.pg_constraint pg_trigger_constr ON pg_trigger_constr.oid=tgconstraint \
               \\n LEFT JOIN pg_catalog.pg_type pg_trigger_constr_type ON pg_trigger_constr_type.oid=pg_trigger_constr.contypid"
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
