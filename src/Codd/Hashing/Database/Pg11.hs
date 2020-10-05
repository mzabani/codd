module Codd.Hashing.Database.Pg11 (Pg11(..), CatalogTablePg11(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison, mapCatTableCol, mapJoinTableTbl, mapColumnComparisonTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..))

data Pg11 = Pg11
newtype CatalogTablePg11 = CatalogTablePg11 CatalogTable
toPg11Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg11
toPg11Col = mapCatTableCol CatalogTablePg11

toPg11Filters :: ([JoinTable Pg10], [ColumnComparison Pg10]) -> ([JoinTable Pg11], [ColumnComparison Pg11])
toPg11Filters (jtbls, wherecols) = (map (mapJoinTableTbl CatalogTablePg11) jtbls, map (mapColumnComparisonTbl CatalogTablePg11) wherecols)

instance DbVersionHash Pg11 where
    type CatTable Pg11 = CatalogTablePg11
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (CatalogTablePg11 tbl, wfilter)

    tableName (CatalogTablePg11 tbl) = tableName @Pg10 tbl

    fqObjNameCol (CatalogTablePg11 tbl) = toPg11Col $ fqObjNameCol @Pg10 tbl

    fqTableIdentifyingCols (CatalogTablePg11 tbl) = map toPg11Col $ fqTableIdentifyingCols @Pg10 tbl

    hashingColsOf (CatalogTablePg11 tbl) = map toPg11Col $ hashingColsOf @Pg10 tbl ++
        case tbl of
            PgConstraint -> [ OidColumn PgConstraint "conparentid" ]
            PgProc -> [ "prokind" ]
            PgAttribute -> [ "atthasmissing", "attmissingval" ]
            _ -> []

    filtersForSchemas includedSchemas = toPg11Filters $ filtersForSchemas @Pg10 includedSchemas
    filtersForRoles includedRoles = toPg11Filters $ filtersForRoles @Pg10 includedRoles

    underSchemaFilter h n = toPg11Filters $ underSchemaFilter @Pg10 h n
    
    underTableFilter h sn tn = toPg11Filters $ underTableFilter @Pg10 h sn tn