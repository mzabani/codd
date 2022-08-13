module Codd.Hashing.Database.Pg14
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..)
                                                , QueryFrag(..)
                                                )
import           Codd.Hashing.Database.Pg10     ( aclArrayTbl
                                                , pronameExpr
                                                , sortArrayExpr
                                                )
import qualified Codd.Hashing.Database.Pg13    as Pg13
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( ChecksumAlgo
                                                , SqlRole
                                                , SqlSchema
                                                , strictRangeCtorOwnership
                                                )
import qualified Database.PostgreSQL.Simple    as DB

-- Postgres 14 introduced multirange types.
hashQueryFor
    :: [SqlRole]
    -> [SqlSchema]
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor allRoles allSchemas checksumAlgo schemaName tableName hobj =
    let hq = Pg13.hashQueryFor allRoles
                               allSchemas
                               checksumAlgo
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
                        [ "pg_language.lanname"
                            , "prosecdef"
                            , "proleakproof"
                            , "proisstrict"
                            , "proretset"
                            , "provolatile"
                            , "proparallel"
                            , "pronargs"
                            , "pronargdefaults"
                            , "prokind"
                            , "pg_type_rettype.typname"
                            , "proargmodes"
                            , "proargnames"
                            , "pg_catalog.pg_get_function_arguments(pg_proc.oid)"
                            , sortArrayExpr "proconfig"
                            , "_codd_roles.permissions"
                            , "CASE WHEN pg_language.lanispl OR pg_language.lanname IN ('sql', 'plpgsql') THEN prosrc END"
                    -- I haven't tested if the same ownership issue that happens to
                    -- range constructors happens to multirange constructors, but I'll be
                    -- conservative and assume it does for now.
                            ]
                            ++ if strictRangeCtorOwnership checksumAlgo
                                   then ["pg_roles.rolname"]
                                   else
                                       [ "CASE WHEN pg_range.rngtypid IS NULL THEN pg_roles.rolname END"
                                       ]
                in
                    HashQuery
                        { objNameCol    = pronameExpr "pg_proc"
                        , checksumCols  = nonAggCols
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
                            <> aclArrayTbl allRoles "proacl"
                            <> "_codd_roles ON TRUE"
                        , nonIdentWhere = Nothing
                        , identWhere    = Just $ "TRUE" <> maybe
                            ""
                            (QueryFrag "\nAND pg_namespace.nspname = ?")
                            (DB.Only <$> schemaName)
                        , groupByCols = ["proname", "proargtypes"] ++ nonAggCols
                        }
            _ -> hq

