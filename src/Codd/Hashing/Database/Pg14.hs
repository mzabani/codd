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
                                                , Include
                                                , SqlRole
                                                , SqlSchema
                                                , strictRangeCtorOwnership
                                                )
import qualified Database.PostgreSQL.Simple    as DB

-- Postgres 14 introduced multirange types.
hashQueryFor
    :: Include SqlRole
    -> Include SqlSchema
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
                    -- , "proargdefaults" -- pg_node_tree type (avoid)
                            , "pg_catalog.pg_get_function_arguments(pg_proc.oid)"
                            , sortArrayExpr "proconfig" -- Not sure what this is, but let's be conservative and sort it meanwhile
                            , "_codd_roles.permissions"
                    -- The source of the function is important, but "prosrc" is _not_ the source if the function
                    -- is compiled, so we ignore this column in those cases.
                    -- Note that this means that functions with different implementations could be considered equal,
                    -- but I don't know a good way around this
                            , "CASE WHEN pg_language.lanispl OR pg_language.lanname IN ('sql', 'plpgsql') THEN prosrc END"
                    -- Only include the owner of the function if this
                    -- is not a range type constructor or if strict-range-ctor-ownership
                    -- is enabled. Read why in DATABASE-EQUALITY.md.
                    -- I haven't tested if the same issue happens to multiranges, but I'll be
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

