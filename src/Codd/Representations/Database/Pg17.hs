module Codd.Representations.Database.Pg17
  ( objRepQueryFor,
  )
where

import Codd.Representations.Database.Model
  ( DbObjRepresentationQuery (..),
  )
import qualified Codd.Representations.Database.Pg16 as Pg16
import Codd.Representations.Types
  ( ObjName,
    ObjectRep (..),
  )
import Codd.Types
  ( SchemaAlgo,
    SchemaSelection,
    SqlRole,
  )

objRepQueryFor ::
  Int ->
  [SqlRole] ->
  SchemaSelection ->
  SchemaAlgo ->
  Maybe ObjName ->
  Maybe ObjName ->
  ObjectRep ->
  DbObjRepresentationQuery
objRepQueryFor serverVersion allRoles allSchemas schemaAlgoOpts schemaName tableName hobj =
  let hq =
        Pg16.objRepQueryFor
          allRoles
          allSchemas
          schemaAlgoOpts
          schemaName
          tableName
          hobj
   in case hobj of
        HCollation ->
          hq
            { repCols =
                [ ("provider", "collprovider"),
                  ( "encoding",
                    "LOWER(pg_catalog.pg_encoding_to_char(pg_collation.collencoding))"
                  ),
                  ("collate", "LOWER(collcollate)"),
                  ("ctype", "LOWER(collctype)"),
                  ("owner", "coll_owner_role.rolname"),
                  ("deterministic", "collisdeterministic"),
                  ("iculocale", "colllocale"), -- This was named colliculocale in Pg16
                  ("icurules", "collicurules")
                ]
            }
        HTableConstraint ->
          hq
            { repCols =
                [ ("type", "pg_constraint.contype"),
                  ("deferrable", "pg_constraint.condeferrable"),
                  ("deferred", "pg_constraint.condeferred"),
                  ("validated", "pg_constraint.convalidated"),
                  ("supporting_index", "pg_class_ind.relname"),
                  ("fk_ref_table", "pg_class_frel.relname"),
                  ("fk_updtype", "pg_constraint.confupdtype"),
                  ("fk_deltype", "pg_constraint.confdeltype"),
                  ("fk_matchtype", "pg_constraint.confmatchtype"),
                  ("local", "pg_constraint.conislocal"),
                  ("inhcount", "pg_constraint.coninhcount"),
                  ("noinherit", "pg_constraint.connoinherit"),
                  -- We don't reference conkey and confkey because the constraint definition will already contain
                  -- referenced columns' names, AND because attnum is affected by dropped columns.
                  --   , "pg_constraint.conkey"
                  --   , "pg_constraint.confkey"
                  -- We check the server version to avoid a bug: https://www.postgresql.org/message-id/flat/a90f53c4-56f3-4b07-aefc-49afdc67dba6%40app.fastmail.com
                  ("definition", if serverVersion <= 170002 then "'not-available-in-pg-17.2-or-less'::text" else "pg_get_constraintdef(pg_constraint.oid, true)"),
                  ( "parent_constraint",
                    "pg_parent_constraint.conname"
                  )
                  -- , "conbin" -- A pg_node_tree
                ]
            }
        HType ->
          hq
            { repCols =
                [ ("nspname", "pg_namespace.nspname"),
                  ("owner", "pg_type_owner.rolname"),
                  ("type", "pg_type.typtype"),
                  ("category", "pg_type.typcategory"),
                  ("preferred", "pg_type.typispreferred"),
                  ("delim", "pg_type.typdelim"),
                  ("notnull", "pg_type.typnotnull"),
                  ("comp_type_table", "pg_class_rel.relname"),
                  ("element_type", "array_element_type.typname"),
                  ("domain_basetype", "pg_type_base.typname"),
                  ("domain_typmod", "pg_type.typtypmod"),
                  ("ndims", "pg_type.typndims"),
                  ("collation", "pg_collation.collname"),
                  ("collation_nsp", "pg_namespace_coll.nspname"),
                  ("default", "pg_type.typdefault"),
                  ("range_subtype", "pg_range_subtype.typname"),
                  ("range_collation", "pg_range_collation.collname"),
                  ("range_collation_nsp", "pg_range_coll_namespace.nspname"),
                  ("range_canonical_function", "pg_range_canonical.proname"),
                  ("range_canonical_nsp", "pg_range_canonical_nsp.nspname"),
                  ("range_subdiff_function", "pg_range_subdiff.proname"),
                  ("range_subdiff_nsp", "pg_range_subdiff_nsp.nspname"),
                  ("opclass", "pg_range_opclass.opcname"),
                  ("opclass_nsp", "pg_range_opclass_nsp.nspname"),
                  ("range_op_class_am", "pg_range_opclass_am.amname"),
                  ("privileges", "typacl.permissions"),
                  ( "composite_type_attrs",
                    "ARRAY_TO_STRING(\
                    \\n ARRAY_AGG(\
                    \\n pg_attribute.attname\
                    \\n || ';' || attribute_type.typname\
                    \\n || ';' || COALESCE(attribute_coll.collname, '')\
                    \\n || ';' || COALESCE(attribute_coll_nsp.nspname, '') ORDER BY pg_attribute.attnum\
                    \\n ), ';')"
                  ),
                  ( "enum_labels",
                    "ARRAY_TO_STRING(ARRAY_AGG(pg_enum.enumlabel::TEXT ORDER BY pg_enum.enumsortorder), ';')"
                  ),
                  -- We check the server version to avoid a bug: https://www.postgresql.org/message-id/flat/a90f53c4-56f3-4b07-aefc-49afdc67dba6%40app.fastmail.com
                  ( "constraints",
                    if serverVersion <= 170002
                      then "ARRAY_TO_STRING(ARRAY_AGG(pg_constraint.convalidated || ';' || pg_constraint.conname ORDER BY pg_constraint.conname), ';')"
                      else
                        "ARRAY_TO_STRING(ARRAY_AGG(pg_constraint.convalidated || ';' || pg_constraint.conname || ';' || pg_get_constraintdef(pg_constraint.oid, true) ORDER BY pg_constraint.conname), ';')"
                  )
                ]
            }
        _ -> hq
