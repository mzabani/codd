module Codd.Hashing.Database.Pg11 (Pg11(..), CatalogTablePg11(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison, mapCatTableCol, mapJoinTableTbl, mapColumnComparisonTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..), CatalogTableAliased(..))

data Pg11 = Pg11
newtype CatalogTablePg11 = CatalogTablePg11 CatalogTableAliased
toPg11Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg11
toPg11Col = mapCatTableCol CatalogTablePg11

toPg11Joins :: [JoinTable Pg10] -> [JoinTable Pg11]
toPg11Joins = map (mapJoinTableTbl CatalogTablePg11)

toPg11Filters :: [ColumnComparison Pg10] -> [ColumnComparison Pg11]
toPg11Filters = map (mapColumnComparisonTbl CatalogTablePg11)

instance DbVersionHash Pg11 where
    type CatTable Pg11 = CatalogTablePg11
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (CatalogTablePg11 tbl, wfilter)

    tableName (CatalogTablePg11 tbl) = tableName @Pg10 tbl

    fqObjNameCol (CatalogTablePg11 tbl) = toPg11Col $ fqObjNameCol @Pg10 tbl

    fqTableIdentifyingCols (CatalogTablePg11 tbl) = map toPg11Col $ fqTableIdentifyingCols @Pg10 tbl

    hashingColsOf (CatalogTablePg11 atbl@(CatalogTableAliased tbl _)) = map toPg11Col $ hashingColsOf @Pg10 atbl ++
        case tbl of
            PgConstraint -> [ OidColumn atbl "conparentid" ]
            PgProc -> [ "prokind" ]
            PgAttribute -> [ "atthasmissing", "attmissingval" ]
            _ -> []

    joinsFor h = toPg11Joins $ joinsFor @Pg10 h

    filtersForSchemas includedSchemas = toPg11Filters $ filtersForSchemas @Pg10 includedSchemas
    filtersForRoles includedRoles = toPg11Filters $ filtersForRoles @Pg10 includedRoles

    underSchemaFilter h n = toPg11Filters $ underSchemaFilter @Pg10 h n
    
    underTableFilter h sn tn = toPg11Filters $ underTableFilter @Pg10 h sn tn