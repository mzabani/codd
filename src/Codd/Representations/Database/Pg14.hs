module Codd.Representations.Database.Pg14
    ( objRepQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..)
                                                , QueryFrag(..)
                                                )
import           Codd.Representations.Database.Pg10
                                                ( aclArrayTbl
                                                , pronameExpr
                                                , sortArrayExpr
                                                )
import qualified Codd.Representations.Database.Pg13
                                               as Pg13
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( SchemaAlgo
                                                , SchemaSelection
                                                , SqlRole
                                                , strictRangeCtorOwnership
                                                )
import qualified Database.PostgreSQL.Simple    as DB

-- Postgres 14 introduced multirange types.
objRepQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> SchemaAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> DbObjRepresentationQuery
objRepQueryFor allRoles allSchemas schemaAlgoOpts schemaName tableName hobj =
    let hq = Pg13.objRepQueryFor allRoles
                                 allSchemas
                                 schemaAlgoOpts
                                 schemaName
                                 tableName
                                 hobj
    in
        case hobj of
            HRoutine ->
                let
                    -- Handling routines is a copy of what's in Pg10.hs, plus
                    -- some changes from Pg11.hs, but comments removed.
                    -- Before changing things here, read comments in both files.
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
                            , ( "args"
                              , "pg_catalog.pg_get_function_arguments(pg_proc.oid)"
                              )
                            , ("config"    , sortArrayExpr "proconfig")
                            , ("privileges", "_codd_roles.permissions")
                            , ( "definition_md5"
                              , "CASE WHEN pg_language.lanispl OR pg_language.lanname IN ('sql', 'plpgsql') THEN MD5(prosrc) END"
                              )
                            , ("kind", "prokind") -- From Pg11.hs
                    -- I haven't tested if the same ownership issue that happens to
                    -- range constructors happens to multirange constructors, but I'll be
                    -- conservative and assume it does for now.
                            ]
                            ++ [ ( "owner"
                                 , if strictRangeCtorOwnership schemaAlgoOpts
                                     then "pg_roles.rolname"
                                     else
                                         "CASE WHEN pg_range.rngtypid IS NULL THEN pg_roles.rolname END"
                                 )
                               ]
                in
                    DbObjRepresentationQuery
                        { objNameCol    = pronameExpr "pg_proc"
                        , repCols       = nonAggCols
                        , fromTable     = "pg_catalog.pg_proc"
                        -- A different JOIN condition for pg_range is one of the important
                        -- changes compared to previous versions
                        , joins         =
                            "JOIN pg_catalog.pg_namespace ON pg_namespace.oid=pronamespace \
                        \\n JOIN pg_catalog.pg_roles ON pg_roles.oid=proowner\
                        \\n LEFT JOIN pg_catalog.pg_depend ON pg_depend.objid=pg_proc.oid\
                        \\n LEFT JOIN pg_catalog.pg_range ON pg_range.rngtypid=pg_depend.refobjid OR pg_range.rngmultitypid=pg_depend.refobjid\
                        \\n LEFT JOIN pg_catalog.pg_language ON pg_language.oid=prolang\
                        \\n LEFT JOIN pg_catalog.pg_type pg_type_rettype ON pg_type_rettype.oid=prorettype\
                        \\n LEFT JOIN pg_catalog.pg_type pg_type_argtypes ON pg_type_argtypes.oid=ANY(proargtypes)\
                        \\n LEFT JOIN LATERAL "
                            <> aclArrayTbl allRoles "'f'" "proowner" "proacl"
                            <> "_codd_roles ON TRUE"
                        , nonIdentWhere = Nothing
                        , identWhere    = Just $ "TRUE" <> maybe
                            ""
                            (QueryFrag "\nAND pg_namespace.nspname = ?")
                            (DB.Only <$> schemaName)
                        , groupByCols   = ["proname", "proargtypes"]
                                              ++ map snd nonAggCols
                        }
            _ -> hq

