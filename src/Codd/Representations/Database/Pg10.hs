module Codd.Representations.Database.Pg10
    ( CustomPrivileges(..)
    , aclArrayTbl
    , objRepQueryFor
    , pronameExpr
    , sortArrayExpr
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..)
                                                , QueryFrag(..)
                                                )
import           Codd.Representations.Database.SqlGen
                                                ( sqlIn )
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( SchemaAlgo(..)
                                                , SchemaSelection(..)
                                                , SqlRole
                                                )
import           Data.Maybe                     ( isJust )
import qualified Database.PostgreSQL.Simple    as DB

data CustomPrivileges = IncludeGrantor | DoNotIncludeGrantor

-- | Returns a one-row relation of type (permissions :: JSONB) with one field/key
-- for every supplied role + the public role, and as values arrays of privileges per grantor. One example result for a database could be:
--                                                                 permissions                                                                 
-- --------------------------------------------------------------------------------------------------------------------------------------------
--  {"public": [["r", "codd-user"]], "codd-user": [["daxr*tDw", "codd_admin"]], "codd_admin": [["r", "codd-user"], ["daxrtDw", "codd_admin"]]}
-- (1 row)
--
-- This format makes it easier for consumers of this function to extract privileges
-- from a specific role or to use privileges from all roles. Naturally, when ignoring
-- grantors the results are slightly different (the role names of grantors will be empty strings).
aclArrayTbl
    :: CustomPrivileges
    -> [SqlRole]
    -> QueryFrag
    -> QueryFrag
    -> QueryFrag
    -> QueryFrag
aclArrayTbl customPrivs allRoles aclKind objOwnerOidIdentifier aclArrayIdentifier
    =
    -- A null array of ACLs actually represents default permissions. See https://github.com/mzabani/codd/issues/117
      "(SELECT jsonb_object_agg(rolname, privsPerGrantor) AS permissions FROM (WITH privs(acl) AS (SELECT unnest(coalesce("
        <> aclArrayIdentifier
        <> ", acldefault("
        <> aclKind
        <> ", "
        <> objOwnerOidIdentifier
        <> "))))"
        <> ", intermediatePrivs (acl, aclRow) AS (SELECT acl, aclexplode(ARRAY[acl]) FROM privs)"
        -- It is possible for the same grantee to have privileges granted by different grantors.
        -- e.g. codd_admin in the table below:

        -- codd-experiments=*> \dp employee
        --                                 Access privileges
        --  Schema |   Name   | Type  |        Access privileges        | Column privileges | Policies 
        -- --------+----------+-------+---------------------------------+-------------------+----------
        --  public | employee | table | codd_admin=arwdDxt/codd_admin  +|                   | 
        --         |          |       | "codd-user"=ar*wdDxt/codd_admin+|                   | 
        --         |          |       | codd_admin=r/"codd-user"        |                   | 
        -- (1 row)

        -- Rebuilding acls with `makeaclitem` and oid `0` for grantee and grantor, then string-aggregating with some simple
        -- text substitution is very laborious. But it is the only way I found to string-extract the letter representations
        -- of privileges while being able to ignore the grantor if desired.
        -- Sadly `aclinsert` throws a "aclinsert is no longer supported" error, and `makeaclitem` doesn't take in multiple privileges
        -- yet (it will after https://commitfest.postgresql.org/38/3653/ makes it to new releases, though, but old versions of postgres
        -- still won't support it).
        <> ", privsGrant (privLetters, granteeOid, grantorOid) AS (SELECT STRING_AGG(TRIM(BOTH FROM makeaclitem(0, 0, (aclRow).privilege_type, (aclRow).is_grantable)::text, '=/0'), '' ORDER BY (aclRow).privilege_type), (aclRow).grantee, (aclRow).grantor\n\
                                                                \FROM intermediatePrivs\n\
                                                                \GROUP BY (aclRow).grantee, (aclRow).grantor)"
        <> "SELECT COALESCE(grantee.rolname, 'public') AS rolname, ARRAY_AGG(ARRAY[privLetters, "
        <> (case customPrivs of
               IncludeGrantor      -> "grantor.rolname"
               DoNotIncludeGrantor -> "''"
           )
        <> "] ORDER BY grantor.rolname) AS privsPerGrantor\n\
                        \FROM privsGrant\n\
                        \LEFT JOIN pg_roles grantee ON grantee.oid=granteeOid\n\
                        \LEFT JOIN pg_roles grantor ON grantor.oid=grantorOid"
        <> "       WHERE granteeOid=0 OR "
        <> "grantee.rolname"
        `sqlIn` allRoles
        <> "       GROUP BY grantee.rolname\n\
                        \ORDER BY grantee.rolname NULLS FIRST) subq)"

oidArrayExpr :: QueryFrag -> QueryFrag -> QueryFrag -> QueryFrag -> QueryFrag
oidArrayExpr oidArrayCol tblToJoin oidColInJoinedTbl aggExpr =
    "(SELECT ARRAY_AGG("
        <> aggExpr
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

pgClassRepQuery
    :: Maybe ([SqlRole], QueryFrag)
            -- ^ If privileges are to be verified, the roles and privileges kind character for the object.
            -- See `acldefault` in https://www.postgresql.org/docs/13/functions-info.html for the kind chars.
    -> Maybe ObjName
    -> DbObjRepresentationQuery
pgClassRepQuery mPrivRolesAndKind schemaName = DbObjRepresentationQuery
    { objNameCol    = "pg_class.relname"
    , repCols       = [ ("rowtype"         , "pg_reltype.typname")
                      , ("compositetype"   , "pg_reloftype.typname")
                      , ("owner"           , "rel_owner_role.rolname")
                      , ("accessmethod"    , "pg_am.amname")
                      , ("shared"          , "pg_class.relisshared")
                      , ("persistence"     , "pg_class.relpersistence")
                      , ("kind"            , "pg_class.relkind")
                      , ("rowsecurity"     , "pg_class.relrowsecurity")
                      , ("forcerowsecurity", "pg_class.relforcerowsecurity")
                      , ("replident"       , "pg_class.relreplident")
                      , ("partition"       , "pg_class.relispartition")
                      , ("amoptions", sortArrayExpr "pg_class.reloptions")
                      -- , "pg_class.relpartbound" -- a pg_node_tree for partition bound, but I couldn't find a function to get its definition
                      ]
                          ++ [ ("privileges", "_codd_roles.permissions")
                             | isJust mPrivRolesAndKind
                             ]
    , fromTable     = "pg_catalog.pg_class"
    , joins         =
        "LEFT JOIN pg_catalog.pg_type pg_reltype ON pg_class.reltype=pg_reltype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_type pg_reloftype ON pg_class.reloftype=pg_reloftype.oid"
        <> "\nLEFT JOIN pg_catalog.pg_roles rel_owner_role ON pg_class.relowner=rel_owner_role.oid"
        <> "\nLEFT JOIN pg_catalog.pg_am ON pg_class.relam=pg_am.oid"
        <> (case mPrivRolesAndKind of
               Nothing -> ""
               Just (allRoles, privilegesKindChar) ->
                   "\nLEFT JOIN LATERAL "
                       <> aclArrayTbl IncludeGrantor
                                      allRoles
                                      privilegesKindChar
                                      "pg_class.relowner"
                                      "pg_class.relacl"
                       <> " _codd_roles ON TRUE"
           )
        <> "\nJOIN pg_catalog.pg_namespace ON pg_class.relnamespace=pg_namespace.oid"
    , nonIdentWhere = Nothing
    , identWhere    = Just $ maybe ""
                                   (QueryFrag "pg_namespace.nspname=?")
                                   (DB.Only <$> schemaName)
    , groupByCols   = []
    }
--   where
--         -- We need to map pg_class.relkind (as per https://www.postgresql.org/docs/13/catalog-pg-class.html)
--         -- to the kind chars that `acldefault` expects, as in https://www.postgresql.org/docs/13/functions-info.html
--     mapRelKindToPrivKind =
--         "(CASE WHEN pg_class.relkind='S' THEN 's' ELSE 'r' END)"

objRepQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> SchemaAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> DbObjRepresentationQuery
objRepQueryFor allRoles schemaSel schemaAlgoOpts schemaName tableName = \case
    HDatabaseSettings ->
        let nonAggCols =
                [ "pg_encoding_to_char(encoding)"
                , "datcollate"
                , "datctype"
                , "rolname"
                ]
        in
            DbObjRepresentationQuery
                { objNameCol    = "datname"
                , repCols       =
                    [ ("encoding", "LOWER(pg_encoding_to_char(encoding))")
                    , ("collate" , "LOWER(datcollate)")
                    , ("ctype"   , "LOWER(datctype)")
                    , ("owner"   , "rolname")
                                  -- We check privileges of roles in each role, but we check privileges of public
                                  -- in db-settings
                    , ( "public_privileges"
                      , "_codd_privs.permissions ->> 'public'"
                      )
                    , ( "settings"
                      , sortArrayExpr
                      $ "ARRAY_AGG(JSONB_BUILD_OBJECT('name', name, 'value', setting, 'min_val', min_val, 'max_val', max_val, 'enum_vals', TO_JSONB("
                      <> sortArrayExpr "enumvals"
                      <> "))"
                                         -- TODO: Should we include pg_settings.reset_val? It is only reflected by `ALTER DATABASE SET ..` in newly opened connections
                      <> " ORDER BY pg_settings.name)"
                      )
                    ]
                , fromTable     = "pg_catalog.pg_database"
                , joins = "JOIN pg_catalog.pg_roles ON datdba = pg_roles.oid "
                          <> "\n LEFT JOIN pg_catalog.pg_settings ON TRUE" -- pg_settings assumes values from the current database
                          <> "\n LEFT JOIN LATERAL "
                          <> aclArrayTbl IncludeGrantor
                                         []
                                         "'d'"
                                         "datdba"
                                         "datacl"
                          <> " _codd_privs ON TRUE "
                , nonIdentWhere =
                    Just
                        "datname = current_database() AND (pg_settings.name IS NULL OR pg_settings.name IN ('default_transaction_isolation', 'default_transaction_deferrable', 'default_transaction_read_only'))" -- TODO: What other settings matter for correctness?
                , identWhere    = Nothing
                , groupByCols   = ["datname", "_codd_privs.permissions"]
                                      ++ nonAggCols
                }
    HSchema -> DbObjRepresentationQuery
        { objNameCol    = "nspname"
        , repCols       = [ ("owner"     , "nsp_owner.rolname")
                          , ("privileges", "_codd_roles.permissions")
                          ]
        , fromTable     = "pg_catalog.pg_namespace"
        , joins         =
            "JOIN pg_catalog.pg_roles AS nsp_owner ON nsp_owner.oid=pg_namespace.nspowner"
            <> "\n LEFT JOIN LATERAL "
            <> aclArrayTbl IncludeGrantor
                           allRoles
                           "'n'"
                           "pg_namespace.nspowner"
                           "nspacl"
            <> "_codd_roles ON TRUE"
        , nonIdentWhere = Just $ case schemaSel of
            IncludeSchemas schemas -> "nspname" `sqlIn` schemas
            AllNonInternalSchemas
                -> "nspname != 'information_schema' AND nspname != 'codd_schema' AND nspname !~ '^pg_'"
        , identWhere    = Nothing
        , groupByCols   = []
        }
    HRole ->
        let
            nonAggCols =
                [ ("superuser"  , "pg_roles.rolsuper")
                , ("inherit"    , "pg_roles.rolinherit")
                , ("createrole" , "pg_roles.rolcreaterole")
                , ("createdb"   , "pg_roles.rolcreatedb")
                , ("canlogin"   , "pg_roles.rolcanlogin")
                , ("replication", "pg_roles.rolreplication")
                , ("bypassrls"  , "pg_roles.rolbypassrls")
                , ("config"     , sortArrayExpr "pg_roles.rolconfig")
                , ( "db_privileges"
                  , "_codd_roles.permissions ->> pg_roles.rolname"
                  )
                ]
        in
            DbObjRepresentationQuery
                { objNameCol    = "pg_roles.rolname"
                , repCols       =
                    nonAggCols
                        ++ [ ( "membership"
                             , "COALESCE(ARRAY_AGG(JSONB_BUILD_OBJECT('role', other_role.rolname, 'admin_option', pg_auth_members.admin_option) ORDER BY other_role.rolname, pg_auth_members.admin_option) FILTER (WHERE other_role.rolname IS NOT NULL), '{}')"
                             )
                           ]
                , fromTable     = "pg_catalog.pg_roles"
                , joins         =
                    "JOIN pg_catalog.pg_database ON pg_database.datname = current_database() \
         \\n LEFT JOIN pg_catalog.pg_auth_members ON pg_auth_members.member=pg_roles.oid \
         \\n LEFT JOIN pg_catalog.pg_roles other_role ON other_role.oid=pg_auth_members.roleid \
         \\n LEFT JOIN LATERAL "
                    <> aclArrayTbl IncludeGrantor
                                   allRoles
                                   "'d'"
                                   "datdba"
                                   "datacl"
                    <> " _codd_roles ON TRUE"
                , nonIdentWhere = Just $ "pg_roles.rolname" `sqlIn` allRoles
                , identWhere    = Nothing
                , groupByCols   = "pg_roles.rolname" : map snd nonAggCols
                }
    HTable ->
        let hq = pgClassRepQuery (Just (allRoles, "'r'")) schemaName
        in  hq { nonIdentWhere = Just "relkind IN ('r', 'f', 'p')" }
    HView ->
        let hq = pgClassRepQuery (Just (allRoles, "'r'")) schemaName
        in
            hq
                { repCols = ("definition_md5", "MD5(pg_views.definition)")
                                : repCols hq
                , joins   =
                    joins hq
                        <> "\nJOIN pg_catalog.pg_views ON pg_views.schemaname=pg_namespace.nspname AND pg_views.viewname=pg_class.relname"
                }
    HPolicy -> DbObjRepresentationQuery
        { objNameCol    = "polname"
        , repCols = [ ("cmd"          , "polcmd")
                    , ("permissive"   , "polpermissive")
                    , ("qual"         , "pg_get_expr(polqual, polrelid)")
                    , ("withcheck"    , "pg_get_expr(polwithcheck, polrelid)")
                    , ("roles_applied", "ARRAY_AGG(rolname ORDER BY rolname)")
                    ]
        , fromTable     = "pg_catalog.pg_policy"
        , joins         =
            "LEFT JOIN pg_catalog.pg_roles ON pg_roles.oid = ANY(polroles)"
            <> "\nJOIN pg_catalog.pg_class ON pg_class.oid = pg_policy.polrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_class.relnamespace = pg_namespace.oid"
        , nonIdentWhere = Just
                          $  "pg_roles.rolname IS NULL OR "
                          <> ("pg_roles.rolname" `sqlIn` allRoles)
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
        let hq = pgClassRepQuery (Just (allRoles, "'s'")) schemaName
        in
            hq
                { repCols = repCols hq
                            ++ [ ("seq_type"   , "pg_seq_type.typname")
                               , ("start"      , "seqstart")
                               , ("increment"  , "seqincrement")
                               , ("max"        , "seqmax")
                               , ("min"        , "seqmin")
                               , ("cache"      , "seqcache")
                               , ("cycle"      , "seqcycle")
                               , ("owner_table", "owner_column.tablename")
                               ]
                                        -- Num instead of name doesn't touch the sequence if the column's renamed,
                                        -- but touches it if the column changes positions (which is probably better)
                            ++ [ ("owner_col_order", "owner_col_order.colorder")
                               | not (ignoreColumnOrder schemaAlgoOpts)
                               ]
                , joins   =
                    joins hq
                        <> "\nJOIN pg_catalog.pg_sequence pg_sequence ON seqrelid=pg_class.oid \
                          \\n JOIN pg_catalog.pg_type AS pg_seq_type ON pg_seq_type.oid=pg_sequence.seqtypid \
                          \\n LEFT JOIN (SELECT pg_depend.objid AS sequence_oid, owner_col_table.oid AS tableid, pg_attribute.attnum, owner_col_table.relname AS tablename, pg_attribute.attnum AS colnum \
                          \\n      FROM pg_catalog.pg_depend \
                          \\n         JOIN pg_catalog.pg_attribute ON pg_attribute.attrelid=pg_depend.refobjid AND pg_attribute.attnum=pg_depend.refobjsubid \
                          \\n         JOIN pg_catalog.pg_class owner_col_table ON owner_col_table.oid=pg_attribute.attrelid) owner_column \
                          \\n            ON owner_column.sequence_oid=pg_class.oid \
                          \\n LEFT JOIN (SELECT attrelid as tableid, attnum, RANK() OVER (PARTITION BY attrelid ORDER BY attnum) AS colorder \
                          \\n      FROM pg_catalog.pg_attribute \
                          \\n      WHERE NOT pg_attribute.attisdropped AND pg_attribute.attname NOT IN ('cmax', 'cmin', 'ctid', 'tableoid', 'xmax', 'xmin')) owner_col_order USING (tableid, attnum)"
                }
    HRoutine ->
        let
            nonAggCols =
                [ ("lang"            , "pg_language.lanname")
                    , ("security_definer", "prosecdef")
                    , ("leakproof"       , "proleakproof")
                    , ("strict"          , "proisstrict")
                    , ("returns_set"     , "proretset")
                    , ("volatile"        , "provolatile")
                    , ("parallel"        , "proparallel")
                    , ("nargs"           , "pronargs")
                    , ("nargs_defaults"  , "pronargdefaults")
                    , ("return_type"     , "pg_type_rettype.typname")
                    , ("argmodes"        , "proargmodes")
                    , ("argnames"        , "proargnames")
                -- , "proargdefaults" -- pg_node_tree type (avoid)
                    , ("args", "pg_catalog.pg_get_function_arguments(pg_proc.oid)")
                    , ("config"          , sortArrayExpr "proconfig") -- Not sure what this is, but let's be conservative and sort it meanwhile

                -- The source of the function is important, but "prosrc" is _not_ the source if the function
                -- is compiled, so we ignore this column in those cases.
                -- Note that this means that functions with different implementations could be considered equal,
                -- but I don't know a good way around this
                    , ( "definition_md5"
                      , "CASE WHEN pg_language.lanispl OR pg_language.lanname IN ('sql', 'plpgsql') THEN MD5(prosrc) END"
                      )
                -- Only include the owner of the function if this
                -- is not a range type constructor or if strict-range-ctor-privs
                -- is enabled. The same applies to grantors of privileges. Read why in DATABASE-EQUALITY.md
                    ]
                    ++ if strictRangeCtorPrivs schemaAlgoOpts
                           then
                               [ ("owner", "pg_roles.rolname")
                               , ( "privileges"
                                 , "_codd_roles_with_grantors.permissions"
                                 )
                               ]
                           else
                               [ ( "owner"
                                 , "CASE WHEN pg_range.rngtypid IS NULL THEN pg_roles.rolname END"
                                 )
                               , ( "privileges"
                                 , "CASE WHEN pg_range.rngtypid IS NULL THEN _codd_roles_with_grantors.permissions ELSE _codd_roles_without_grantors.permissions END"
                                 )
                               ]
        in
            DbObjRepresentationQuery
                { objNameCol    = pronameExpr "pg_proc"
                , repCols       = nonAggCols
                , fromTable     = "pg_catalog.pg_proc"
                , joins         =
                    "JOIN pg_catalog.pg_namespace ON pg_namespace.oid=pronamespace \
                 \\n JOIN pg_catalog.pg_roles ON pg_roles.oid=proowner\
                 \\n LEFT JOIN pg_catalog.pg_depend ON pg_depend.objid=pg_proc.oid\
                 \\n LEFT JOIN pg_catalog.pg_range ON pg_range.rngtypid=pg_depend.refobjid\
                 \\n LEFT JOIN pg_catalog.pg_language ON pg_language.oid=prolang\
                 \\n LEFT JOIN pg_catalog.pg_type pg_type_rettype ON pg_type_rettype.oid=prorettype\
                 \\n LEFT JOIN pg_catalog.pg_type pg_type_argtypes ON pg_type_argtypes.oid=ANY(proargtypes)\n"
                 -- We join to privileges both with and without grantors, so we can decide at runtime
                 -- which one to use. It would be nice if we could json-map over the results _with_ grantors
                -- to remove them if necessary, but that's beyond what I know to do with json functions in postgres
                    <> "LEFT JOIN LATERAL "
                    <> aclArrayTbl IncludeGrantor
                                   allRoles
                                   "'f'"
                                   "proowner"
                                   "proacl"
                    <> "_codd_roles_with_grantors ON TRUE\
                \\n LEFT JOIN LATERAL "
                    <> aclArrayTbl DoNotIncludeGrantor
                                   allRoles
                                   "'f'"
                                   "proowner"
                                   "proacl"
                    <> "_codd_roles_without_grantors ON TRUE"
                , nonIdentWhere = Nothing
                , identWhere    = Just $ "TRUE" <> maybe
                                      ""
                                      (QueryFrag "\nAND pg_namespace.nspname = ?")
                                      (DB.Only <$> schemaName)
                , groupByCols = ["proname", "proargtypes"] ++ map snd nonAggCols
                }
    HColumn -> DbObjRepresentationQuery
        { objNameCol    = "attname"
        , repCols       =
            [ ("type"      , "pg_type.typname")
                , ("notnull"   , "attnotnull")
                , ("hasdefault", "atthasdef")
                , ( "default"
                  , "pg_catalog.pg_get_expr(pg_attrdef.adbin, pg_attrdef.adrelid)"
                  )
                , ("identity"     , "attidentity")
                , ("local"        , "attislocal")
                , ("inhcount"     , "attinhcount")
                , ("typmod"       , "atttypmod")
                , ("collation"    , "pg_collation.collname")
                , ("collation_nsp", "collation_namespace.nspname")
                , ("privileges"   , "_codd_roles.permissions")
            -- It's not clear what attoptions and attfdwoptions represent, so we're not including them yet
            -- , sortArrayExpr "attoptions" 
            -- , sortArrayExpr "attfdwoptions"
            -- , "attnum" -- We use a window function instead of attnum because the latter is affected by dropped columns!
            --               But only if ignore-column-order is not set
                ]
                ++ [ ( "order"
                     , "RANK() OVER (PARTITION BY pg_attribute.attrelid ORDER BY pg_attribute.attnum)"
                     )
                   | not (ignoreColumnOrder schemaAlgoOpts)
                   ]
        , fromTable     = "pg_catalog.pg_attribute"
        , joins         =
            "JOIN pg_catalog.pg_class ON pg_class.oid=attrelid"
            <> "\nJOIN pg_catalog.pg_namespace ON pg_namespace.oid=pg_class.relnamespace"
            <> "\nLEFT JOIN pg_catalog.pg_type ON pg_type.oid=atttypid"
            <> "\nLEFT JOIN pg_catalog.pg_attrdef ON pg_attrdef.adrelid=pg_class.oid AND pg_attrdef.adnum=pg_attribute.attnum"
            <> "\nLEFT JOIN pg_collation ON pg_collation.oid=pg_attribute.attcollation"
            <> "\nLEFT JOIN pg_namespace collation_namespace ON pg_collation.collnamespace=collation_namespace.oid"
            <> "\n LEFT JOIN LATERAL "
            <> aclArrayTbl IncludeGrantor
                           allRoles
                           "'c'"
                           "pg_class.relowner"
                           "attacl"
            <> "_codd_roles ON TRUE"
        , nonIdentWhere = Just
            "NOT pg_attribute.attisdropped AND pg_attribute.attnum >= 1"
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
    HTableConstraint -> DbObjRepresentationQuery
        { objNameCol    = constraintnameExpr "pg_constraint" "pg_domain_type"
        , repCols = [ ("type"            , "pg_constraint.contype")
                    , ("deferrable"      , "pg_constraint.condeferrable")
                    , ("deferred"        , "pg_constraint.condeferred")
                    , ("validated"       , "pg_constraint.convalidated")
                    , ("supporting_index", "pg_class_ind.relname")
                    , ("fk_ref_table"    , "pg_class_frel.relname")
                    , ("fk_updtype"      , "pg_constraint.confupdtype")
                    , ("fk_deltype"      , "pg_constraint.confdeltype")
                    , ("fk_matchtype"    , "pg_constraint.confmatchtype")
                    , ("local"           , "pg_constraint.conislocal")
                    , ("inhcount"        , "pg_constraint.coninhcount")
                    , ("noinherit"       , "pg_constraint.connoinherit")
                          -- We don't reference conkey and confkey because the constraint definition will already contain
                          -- referenced columns' names, AND because attnum is affected by dropped columns.
                        --   , "pg_constraint.conkey"
                        --   , "pg_constraint.confkey"
                    , ("definition", "pg_get_constraintdef(pg_constraint.oid)")
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
        let hq = pgClassRepQuery Nothing schemaName
        in
            hq
                {
            -- TODO: Lots of columns still missing!! But pg_get_indexdef should do a good enough job for now
                  repCols    =
                    repCols hq
                        ++ [ ("unique"   , "indisunique")
                           , ("primary"  , "indisprimary")
                           , ("exclusion", "indisexclusion")
                           , ("immediate", "indimmediate")
                           , ( "definition"
                             , "pg_get_indexdef(pg_index.indexrelid)"
                             )
                                        -- , "indexprs" -- pg_node_tree
                                        -- , "indpred" -- pg_node_tree
                           ]
                , joins      =
                    joins hq
                        <> "\n JOIN pg_catalog.pg_index ON pg_index.indexrelid=pg_class.oid \
                                  \\n JOIN pg_catalog.pg_class pg_index_table ON pg_index.indrelid=pg_index_table.oid"
                , identWhere =
                    (<> maybe ""
                              (QueryFrag " AND pg_index_table.relname=?")
                              (DB.Only <$> tableName)
                        )
                        <$> identWhere hq
                }
    HTrigger -> DbObjRepresentationQuery
        { objNameCol    = "tgname"
        , repCols       =
            [ ("function"        , pronameExpr "pg_proc")
            , ("type"            , "tgtype")
            , ("enabled"         , "tgenabled")
            , ("table_ref"       , "pg_ref_table.relname")
            , ("supporting_index", "pg_trigger_ind.relname")
            , ( "associated_constraint"
              , constraintnameExpr "pg_trigger_constr" "pg_trigger_constr_type"
              )
            , ("deferrable"  , "tgdeferrable")
            , ("initdeferred", "tginitdeferred")
            , ("nargs"       , "tgnargs")
                        -- Neither of the columns below seem to be useful, and one of them references column attrnums,
                        -- which we don't want
                        --   , "tgattr"
                        --   , "tgargs"
                          -- , "tgqual" -- This is system dependent. Equal expression can have different pg_node_tree::text representations
                          -- With the inclusion below, many other columns are probably unnecessary
            , ("definition", "pg_catalog.pg_get_triggerdef(pg_trigger.oid)")
            , ("oldtblname"  , "tgoldtable")
            , ("newtblname"  , "tgnewtable")
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

    HCollation -> DbObjRepresentationQuery
        { objNameCol    = "collname"
        , repCols       =
            [ ("provider", "collprovider")
                , ( "encoding"
                  , "LOWER(pg_catalog.pg_encoding_to_char(pg_collation.collencoding))"
                  )
                , ("collate", "LOWER(collcollate)")
                , ("ctype"  , "LOWER(collctype)")
                , ("owner"  , "coll_owner_role.rolname")
                ]
                ++ if strictCollations schemaAlgoOpts
                       then
                           [
                           -- Read more about collation schema representations in DATABASE-EQUALITY.md
                             ("version", "collversion")
                           , ( "actual_version"
                             , "pg_catalog.pg_collation_actual_version(pg_collation.oid)"
                             )
                           ]
                       else []
        , fromTable     = "pg_catalog.pg_collation"
        , joins         =
            "LEFT JOIN pg_catalog.pg_roles coll_owner_role ON collowner=coll_owner_role.oid \
         \\n LEFT JOIN pg_catalog.pg_namespace ON collnamespace=pg_namespace.oid"
        , nonIdentWhere = Nothing
        , identWhere    = Just $ "TRUE" <> maybe
                              ""
                              (QueryFrag "\nAND pg_namespace.nspname = ?")
                              (DB.Only <$> schemaName)
        , groupByCols   = []
        }

    HType ->
        -- TODO: Base types and shell types
        let ckCols =
                [ ("nspname"                 , "pg_namespace.nspname")
                , ("owner"                   , "pg_type_owner.rolname")
                , ("type"                    , "pg_type.typtype")
                , ("category"                , "pg_type.typcategory")
                , ("preferred"               , "pg_type.typispreferred")
                , ("delim"                   , "pg_type.typdelim")
                , ("notnull"                 , "pg_type.typnotnull")
                , ("comp_type_table"         , "pg_class_rel.relname")
                , ("element_type"            , "array_element_type.typname")
                , ("domain_basetype"         , "pg_type_base.typname")
                , ("domain_typmod"           , "pg_type.typtypmod")
                , ("ndims"                   , "pg_type.typndims")
                , ("collation"               , "pg_collation.collname")
                , ("collation_nsp"           , "pg_namespace_coll.nspname")
                , ("default"                 , "pg_type.typdefault")
                , ("range_subtype"           , "pg_range_subtype.typname")
                , ("range_collation"         , "pg_range_collation.collname")
                , ("range_collation_nsp", "pg_range_coll_namespace.nspname")
                , ("range_canonical_function", "pg_range_canonical.proname")
                , ("range_canonical_nsp"     , "pg_range_canonical_nsp.nspname")
                , ("range_subdiff_function"  , "pg_range_subdiff.proname")
                , ("range_subdiff_nsp"       , "pg_range_subdiff_nsp.nspname")
                , ("opclass"                 , "pg_range_opclass.opcname")
                , ("opclass_nsp"             , "pg_range_opclass_nsp.nspname")
                , ("range_op_class_am"       , "pg_range_opclass_am.amname")
                , ("privileges"              , "typacl.permissions")
                ]
        in
            DbObjRepresentationQuery
                { objNameCol    = "pg_type.typname"
                , repCols       =
                    ckCols
                        ++ [ ( "composite_type_attrs"
                             , "ARRAY_TO_STRING(\
              \\n ARRAY_AGG(\
                \\n pg_attribute.attname\
                \\n || ';' || attribute_type.typname\
                \\n || ';' || COALESCE(attribute_coll.collname, '')\
                \\n || ';' || COALESCE(attribute_coll_nsp.nspname, '') ORDER BY pg_attribute.attnum\
              \\n ), ';')"
                             )
                           , ( "enum_labels"
                             , "ARRAY_TO_STRING(ARRAY_AGG(pg_enum.enumlabel::TEXT ORDER BY pg_enum.enumsortorder), ';')"
                             )
                           , ( "constraints"
                             , "ARRAY_TO_STRING(ARRAY_AGG(pg_constraint.convalidated || ';' || pg_constraint.conname || ';' || pg_get_constraintdef(pg_constraint.oid) ORDER BY pg_constraint.conname), ';')"
                             )
                           ]
                , fromTable     = "pg_catalog.pg_type"
                , joins         =
                    "LEFT JOIN pg_catalog.pg_namespace ON typnamespace=pg_namespace.oid\
\\nLEFT JOIN pg_catalog.pg_roles pg_type_owner ON pg_type_owner.oid=typowner\
\\nLEFT JOIN pg_catalog.pg_class pg_class_rel ON pg_class_rel.oid=pg_type.typrelid\
\\nLEFT JOIN pg_catalog.pg_type array_element_type ON array_element_type.oid=pg_type.typelem\
\\nLEFT JOIN pg_catalog.pg_type pg_type_base ON pg_type_base.oid=pg_type.typbasetype\
\\nLEFT JOIN pg_catalog.pg_collation ON pg_collation.oid=pg_type.typcollation\
\\nLEFT JOIN pg_catalog.pg_namespace pg_namespace_coll ON pg_namespace_coll.oid=collnamespace"
                    <>
    -- We can't group by typacl because the planner errors with 'Some of the datatypes only support hashing, while others only support sorting.'
                       "\nLEFT JOIN LATERAL "
                    <> aclArrayTbl IncludeGrantor
                                   allRoles
                                   "'T'"
                                   "pg_type.typowner"
                                   "pg_type.typacl"
                    <> " typacl ON TRUE"
                    <>
    -- Joins for attributes of composite types
                       "\nLEFT JOIN pg_catalog.pg_attribute ON pg_attribute.attrelid=pg_class_rel.oid\
\\nLEFT JOIN pg_catalog.pg_type attribute_type ON attribute_type.oid=pg_attribute.atttypid\
\\nLEFT JOIN pg_catalog.pg_collation attribute_coll ON attribute_coll.oid=pg_attribute.attcollation\
\\nLEFT JOIN pg_catalog.pg_namespace attribute_coll_nsp ON attribute_coll_nsp.oid=attribute_coll.collnamespace"
                    <>
    -- Joins for enum types
                       "\nLEFT JOIN pg_catalog.pg_enum ON pg_enum.enumtypid=pg_type.oid"
                    <>
    -- Joins for range types
                       "\nLEFT JOIN pg_catalog.pg_range ON pg_type.oid=pg_range.rngtypid\
\\nLEFT JOIN pg_catalog.pg_type pg_range_subtype ON pg_range_subtype.oid=pg_range.rngsubtype\
\\nLEFT JOIN pg_catalog.pg_collation pg_range_collation ON pg_range_collation.oid=pg_range.rngcollation\
\\nLEFT JOIN pg_catalog.pg_namespace pg_range_coll_namespace ON pg_range_coll_namespace.oid=pg_range_collation.collnamespace\
\\nLEFT JOIN pg_catalog.pg_proc pg_range_canonical ON pg_range_canonical.oid=pg_range.rngcanonical\
\\nLEFT JOIN pg_catalog.pg_namespace pg_range_canonical_nsp ON pg_range_canonical_nsp.oid=pg_range_canonical.pronamespace\
\\nLEFT JOIN pg_catalog.pg_proc pg_range_subdiff ON pg_range_subdiff.oid=pg_range.rngsubdiff\
\\nLEFT JOIN pg_catalog.pg_namespace pg_range_subdiff_nsp ON pg_range_subdiff_nsp.oid=pg_range_subdiff.pronamespace\
\\nLEFT JOIN pg_catalog.pg_opclass pg_range_opclass ON pg_range_opclass.oid=pg_range.rngsubopc\
\\nLEFT JOIN pg_catalog.pg_namespace pg_range_opclass_nsp ON pg_range_opclass_nsp.oid=pg_range_opclass.opcnamespace\
\\nLEFT JOIN pg_catalog.pg_am pg_range_opclass_am ON pg_range_opclass_am.oid=pg_range_opclass.opcmethod"
                    <>
    -- Joins for domain types
                       "\nLEFT JOIN pg_catalog.pg_constraint ON pg_constraint.contypid=pg_type.oid"
                , nonIdentWhere =
                    Just
                -- Postgres creates an array type for each user-defined type and one type
                -- for each table, view, sequence and possibly others - alongside one extra array type
                -- for each one of these as well.
                -- A few thoughts:
                -- 1 - We hope/assume which array types get automatically created follows the same criteria
                --     for different PG versions. Reading the docs from versions 10 to 14 this seems to be true,
                --     look for "Whenever a user-defined type is created" in https://www.postgresql.org/docs/10/sql-createtype.html.
                --     It also seems privileges can't be granted to array types, so this means we can disregard array types completely.
                -- 2 - Types generated per tables, views and other relations are redundant so we don't include
                --     either those or their array types. They can't be removed because views and tables depend on them,
                --     so that keeps us safe.
                --     TODO: actually, it seems possible to grant privileges to table types. This _could_ mean we should include them in 
                --     the expected schema, at least when their types differ from the default.
                        "pg_type.typisdefined AND array_element_type.oid IS NULL AND (pg_class_rel.relkind IS NULL OR pg_class_rel.relkind = 'c')"
                , identWhere    = Just $ QueryFrag "pg_namespace.nspname = ?"
                                                   (DB.Only schemaName)
                , groupByCols   =
                    map snd ckCols
                        ++ [ "pg_type.typname"
                           , "pg_type.typelem"
                           , "pg_type.typlen"
                           ]
                }
        -- === Do not include:
        -- typlen and typbyval, since they're machine-dependent
        -- typarray not necessary since it merely represents the existence of another type
        -- typalign, typstorage
        -- === To understand:
        -- typrelid (what's a free-standing composite type?)
        -- typtypmod (what's a typmod that's applied to a base type?)
        -- typndims (is this necessary if we check the base type by name?)
        -- === Filter by:
        -- typisdefined=true (also investigate what this means, is this related to shell types?)
