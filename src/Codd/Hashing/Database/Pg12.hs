module Codd.Hashing.Database.Pg12 (Pg12) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinFilter(..), CatTable(..), mapCatTableCol, mapJoinFilterTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..))

data Pg12
newtype CatalogTablePg12 = CatalogTablePg12 CatalogTable
toPg12Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg12
toPg12Col = mapCatTableCol CatalogTablePg12

toPg12JoinFilter :: JoinFilter Pg10 -> JoinFilter Pg12
toPg12JoinFilter = mapJoinFilterTbl CatalogTablePg12

instance DbVersionHash Pg12 where
    type CatTable Pg12 = CatalogTablePg12
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (CatalogTablePg12 tbl, wfilter)

    tableName (CatalogTablePg12 tbl) = tableName @Pg10 tbl

    fqObjNameCol (CatalogTablePg12 tbl) = toPg12Col $ fqObjNameCol @Pg10 tbl

    fqTableIdentifyingCols (CatalogTablePg12 tbl) = map toPg12Col $ fqTableIdentifyingCols @Pg10 tbl

    hashingColsOf (CatalogTablePg12 tbl) = map toPg12Col $ hashingColsOf @Pg10 tbl ++
        case tbl of
            PgConstraint -> [ JoinOid PgConstraint "conparentid" ]
            PgAttribute -> [ "attgenerated", "atthasmissing", "attmissingval" ]
            PgProc -> [ "prokind" ]
            _ -> []

    underSchemaFilter h n = map toPg12JoinFilter $ underSchemaFilter @Pg10 h n
    
    underTableFilter h n = map toPg12JoinFilter $ underTableFilter @Pg10 h n