CREATE EXTENSION pg_abris CASCADE;

SELECT title,
    primarykey,
    ems_en ,
    entity ,
    base_entity 
 FROM meta.entity
 ORDER BY entity;
SELECT * FROM meta.projection_entity order by 1,2,3,4,5,6,7,8,9,10,11;
SELECT * FROM meta.relation order by 1,2,3,4,5,6,7,8;
SELECT * FROM meta.projection_relation order by 1,2,3,4,5,6,7,8,9,10,11,12,13;
SELECT * FROM meta.tables order by 1,2,3,4,5,6,7,8,9,10,11;
SELECT * FROM  meta.base_entity order by 1,2,3;
SELECT * FROM  meta.base_table order by 1,2;
SELECT * FROM  meta.columns order by 1,2,3,4,5;
SELECT * FROM  meta.constraint_column_usage  order by 1,2,3,4,5,6,7;
SELECT * FROM  meta.constraint_column_usage_ex   order by 1,2,3,4,5,6,7,8,9;
SELECT * FROM  meta.entity_type   order by 1,2;
SELECT function_name, function_schema FROM  meta.functions order by 1,2;
SELECT * FROM  meta.menu  order by 1,2,3,4,5,6,7,8,9,10;
SELECT * FROM  meta.page_block_layout  order by 1,2;
SELECT * FROM  meta.property order by 1; 
SELECT * FROM  meta.property_add  order by 1;
SELECT * FROM  meta.projection_property  order by 1;
SELECT * FROM  meta.projection_relation_ex  order by 1;
SELECT * FROM  meta.property_type  order by 1; 
SELECT * FROM  meta.reference  order by 1,2,3,4,5;
SELECT * FROM  meta.schema  order by 1;
SELECT * FROM  meta.table_relation  order by 1,2,3,4,5,6; 
SELECT * FROM  meta.user_list  order by 1;
SELECT * FROM  meta.view_entity  order by 1,2,3,4,5,6;
SELECT * FROM  meta.view_page  order by 1, 2;
 
 
 
 