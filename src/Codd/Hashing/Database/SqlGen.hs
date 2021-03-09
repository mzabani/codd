module Codd.Hashing.Database.SqlGen where

import           Codd.Hashing.Database.Model    ( QueryFrag(..) )
import           Codd.Types                     ( Include(..) )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.ToField
                                               as DB

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
