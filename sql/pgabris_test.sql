CREATE EXTENSION pgabris CASCADE;

SELECT title,
    primarykey,
    ems_en ,
    entity ,
    base_entity 
 FROM meta.entity
 ORDER BY entity;
 
 SELECT * FROM meta.projection_entity;


 SELECT * FROM meta.relation;
 
 SELECT * FROM meta.projection_relation;
 
 SELECT * FROM meta.tables;
 
SELECT * FROM  meta.base_entity;
 
SELECT * FROM  meta.base_table;
 
SELECT * FROM  meta.columns;
 
SELECT * FROM  meta.constraint_column_usage;
 
SELECT * FROM  meta.constraint_column_usage_ex;
 
SELECT * FROM  meta.entity_type;
 
SELECT function_name FROM  meta.functions;
 
SELECT * FROM  meta.menu;
 
SELECT * FROM  meta.page_block_layout;
 
SELECT * FROM  meta.property; 
 
SELECT * FROM  meta.property_add;
 
SELECT * FROM  meta.projection_property;
 
SELECT * FROM  meta.projection_relation_ex;
 
SELECT * FROM  meta.property_type; 
 
--SELECT * FROM  meta.table_constraints;
 
SELECT * FROM  meta.reference;
 
 
SELECT * FROM  meta.schema;
 
SELECT * FROM  meta.table_relation; 
 
SELECT * FROM  meta.user_list;
 
SELECT * FROM  meta.view_entity;
 
SELECT * FROM  meta.view_page;
 
 
 
 