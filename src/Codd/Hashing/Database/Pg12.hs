module Codd.Hashing.Database.Pg12 (Pg12(..)) where

import Codd.Hashing.Database.Model (DbVersionHash(..), CatTableAliased(..), CatalogTableColumn(..), JoinTable(..), CatTable(..), ColumnComparison, mapCatTableAliased, mapCatTableCol, mapJoinTableTbl, mapColumnComparisonTbl)
import Codd.Hashing.Database.Pg10 (Pg10, CatalogTable(..))

data Pg12 = Pg12
newtype CatalogTablePg12 = CatalogTablePg12 CatalogTable

toTblAliased :: CatTableAliased Pg10 -> CatTableAliased Pg12
toTblAliased = mapCatTableAliased CatalogTablePg12

toPg12Col :: CatalogTableColumn Pg10 -> CatalogTableColumn Pg12
toPg12Col = mapCatTableCol toTblAliased

toPg12Joins :: [JoinTable Pg10] -> [JoinTable Pg12]
toPg12Joins = map (mapJoinTableTbl toTblAliased)

toPg12Filters :: [ColumnComparison Pg10] -> [ColumnComparison Pg12]
toPg12Filters = map (mapColumnComparisonTbl toTblAliased)

instance DbVersionHash Pg12 where
    type CatTable Pg12 = CatalogTablePg12
    hashableObjCatalogTable hobj = let (tbl, wfilter) = hashableObjCatalogTable @Pg10 hobj in (toTblAliased tbl, wfilter)

    tableName (CatTableAliased (CatalogTablePg12 tbl) alias) = tableName $ CatTableAliased @Pg10 tbl alias

    fqObjNameCol (CatTableAliased (CatalogTablePg12 tbl) alias) = toPg12Col $ fqObjNameCol (CatTableAliased @Pg10 tbl alias)

    fqTableIdentifyingCols (CatTableAliased (CatalogTablePg12 tbl) alias) = map toPg12Col $ fqTableIdentifyingCols (CatTableAliased @Pg10 tbl alias)

    hashingColsOf (CatTableAliased (CatalogTablePg12 tbl) alias) =
        let
            pg10tbl = CatTableAliased @Pg10 tbl alias
        in
            map toPg12Col $ hashingColsOf pg10tbl ++
                case tbl of
                    PgConstraint -> [ OidColumn pg10tbl "conparentid" ]
                    PgAttribute -> [ "attgenerated", "atthasmissing", "attmissingval" ]
                    PgProc -> [ "prokind" ]
                    _ -> []

    joinsFor h = toPg12Joins $ joinsFor @Pg10 h
    filtersForSchemas includedSchemas = toPg12Filters $ filtersForSchemas @Pg10 includedSchemas
    filtersForRoles includedRoles = toPg12Filters $ filtersForRoles @Pg10 includedRoles

    underSchemaFilter h n = toPg12Filters $ underSchemaFilter @Pg10 h n
    
    underTableFilter h sn tn = toPg12Filters $ underTableFilter @Pg10 h sn tn