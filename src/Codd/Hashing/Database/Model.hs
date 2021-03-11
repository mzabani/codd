module Codd.Hashing.Database.Model where

import           Data.String                    ( IsString(..) )
import           Database.PostgreSQL.Simple     ( Query
                                                , ToRow
                                                )
import qualified Database.PostgreSQL.Simple    as DB

data QueryFrag = forall a . ToRow a => QueryFrag Query a
instance IsString QueryFrag where
  fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
  QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 DB.:. p2)
instance Monoid QueryFrag where
  mempty = ""
instance Show QueryFrag where
  show (QueryFrag q _) = show q

withQueryFrag :: QueryFrag -> (forall a . ToRow a => Query -> a -> b) -> b
withQueryFrag (QueryFrag q args) f = f q args

data HashQuery = HashQuery
  { objNameCol    :: QueryFrag
  , checksumCols  :: [QueryFrag]
  , fromTable     :: QueryFrag
  , joins         :: QueryFrag
  , nonIdentWhere :: Maybe QueryFrag
  , identWhere    :: Maybe QueryFrag
  , groupByCols   :: [QueryFrag]
  }
