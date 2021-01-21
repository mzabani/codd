module Codd.Hashing.Database.Pg11 (Pg11(..), CatalogTablePg11(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatTableAliased(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison, mapCatTableAliased, mapCatTableCol, mapJoinTableTbl, mapColumnComparisonTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..))

data Pg11 = Pg11
newtype CatalogTablePg11 = CatalogTablePg11 CatalogTable

toTblAliased :: CatTableAliased Pg10 -> CatTableAliased Pg11
toTblAliased = mapCatTableAliased CatalogTablePg11

toPg11Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg11
toPg11Col = mapCatTableCol toTblAliased

toPg11Joins :: [JoinTable Pg10] -> [JoinTable Pg11]
toPg11Joins = map (mapJoinTableTbl toTblAliased)

toPg11Filters :: [ColumnComparison Pg10] -> [ColumnComparison Pg11]
toPg11Filters = map (mapColumnComparisonTbl toTblAliased)

instance DbVersionHash Pg11 where
    type CatTable Pg11 = CatalogTablePg11
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (toTblAliased tbl, wfilter)

    tableName (CatTableAliased (CatalogTablePg11 tbl) alias) = tableName $ CatTableAliased @Pg10 tbl alias

    fqObjNameCol (CatTableAliased (CatalogTablePg11 tbl) alias) = toPg11Col $ fqObjNameCol (CatTableAliased @Pg10 tbl alias)

    fqTableIdentifyingCols (CatTableAliased (CatalogTablePg11 tbl) alias) = map toPg11Col $ fqTableIdentifyingCols (CatTableAliased @Pg10 tbl alias)

    hashingColsOf (CatTableAliased (CatalogTablePg11 tbl) alias) = 
        let
            pg10tbl = CatTableAliased @Pg10 tbl alias
        in
            map toPg11Col $ hashingColsOf pg10tbl ++
                case tbl of
                    PgConstraint -> [ OidColumn pg10tbl "conparentid" ]
                    PgProc -> [ "prokind" ]
                    PgAttribute -> [ "atthasmissing", "attmissingval" ]
                    _ -> []

    joinsFor h = toPg11Joins $ joinsFor @Pg10 h

    filtersForSchemas includedSchemas = toPg11Filters $ filtersForSchemas @Pg10 includedSchemas
    filtersForRoles includedRoles = toPg11Filters $ filtersForRoles @Pg10 includedRoles

    underSchemaFilter h n = toPg11Filters $ underSchemaFilter @Pg10 h n
    
    underTableFilter h sn tn = toPg11Filters $ underTableFilter @Pg10 h sn tn