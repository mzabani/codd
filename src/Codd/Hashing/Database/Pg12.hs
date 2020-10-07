module Codd.Hashing.Database.Pg12 (Pg12(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison, mapCatTableCol, mapJoinTableTbl, mapColumnComparisonTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..))

data Pg12 = Pg12
newtype CatalogTablePg12 = CatalogTablePg12 CatalogTable
toPg12Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg12
toPg12Col = mapCatTableCol CatalogTablePg12

toPg12Joins :: [JoinTable Pg10] -> [JoinTable Pg12]
toPg12Joins = map (mapJoinTableTbl CatalogTablePg12)

toPg12Filters :: [ColumnComparison Pg10] -> [ColumnComparison Pg12]
toPg12Filters = map (mapColumnComparisonTbl CatalogTablePg12)

instance DbVersionHash Pg12 where
    type CatTable Pg12 = CatalogTablePg12
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (CatalogTablePg12 tbl, wfilter)

    tableName (CatalogTablePg12 tbl) = tableName @Pg10 tbl

    fqObjNameCol (CatalogTablePg12 tbl) = toPg12Col $ fqObjNameCol @Pg10 tbl

    fqTableIdentifyingCols (CatalogTablePg12 tbl) = map toPg12Col $ fqTableIdentifyingCols @Pg10 tbl

    hashingColsOf (CatalogTablePg12 tbl) = map toPg12Col $ hashingColsOf @Pg10 tbl ++
        case tbl of
            PgConstraint -> [ OidColumn PgConstraint "conparentid" ]
            PgAttribute -> [ "attgenerated", "atthasmissing", "attmissingval" ]
            PgProc -> [ "prokind" ]
            _ -> []

    joinsFor h = toPg12Joins $ joinsFor @Pg10 h
    filtersForSchemas includedSchemas = toPg12Filters $ filtersForSchemas @Pg10 includedSchemas
    filtersForRoles includedRoles = toPg12Filters $ filtersForRoles @Pg10 includedRoles

    underSchemaFilter h n = toPg12Filters $ underSchemaFilter @Pg10 h n
    
    underTableFilter h sn tn = toPg12Filters $ underTableFilter @Pg10 h sn tn