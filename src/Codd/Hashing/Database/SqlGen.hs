module Codd.Hashing.Database.SqlGen
    ( safeStringConcat
    , interspBy
    , includeSql
    , parens
    ) where

import           Codd.Hashing.Database.Model    ( QueryFrag(..) )
import           Codd.Types                     ( Include(..) )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.ToField
                                               as DB

-- | Given a list of possibly non-text and possibly NULL expressions, returns
-- an expression that is not null and that guarantees distinct values for
-- any of the supplied expressions yield a different result.
safeStringConcat :: [QueryFrag] -> QueryFrag
safeStringConcat exprs = interspBy False " || " (map coalesce exprs)
  where
    coalesce expr =
        "CASE WHEN "
            <> expr
            <> " IS NULL THEN '' ELSE '_' || ("
            <> expr
            <> ")::TEXT END"

interspBy :: Bool -> QueryFrag -> [QueryFrag] -> QueryFrag
interspBy _  _ []  = ""
interspBy ps _ [c] = if ps then parens c else c
interspBy ps sep (c : cs) =
    (if ps then parens c else c) <> sep <> interspBy ps sep cs

parens :: QueryFrag -> QueryFrag
parens q = "(" <> q <> ")"

-- | Generates an "IN" or "NOT IN" for the supplied table's object's name column.
includeSql :: DB.ToField b => Include b -> QueryFrag -> QueryFrag
includeSql inc sqlexpr = case inc of
    Include [] -> "FALSE"
    Exclude [] -> "TRUE"
    Exclude objNames ->
        sqlexpr <> QueryFrag " NOT IN ?" (DB.Only $ DB.In objNames)
    Include objNames -> sqlexpr <> QueryFrag " IN ?" (DB.Only $ DB.In objNames)
    IncludeExclude incNames excNames ->
        includeSql (Include incNames) sqlexpr
            <> " AND "
            <> includeSql (Exclude excNames) sqlexpr
