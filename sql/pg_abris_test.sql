CREATE EXTENSION pg_abris CASCADE;


CREATE TABLE table1 (
    key uuid NOT NULL,
    row1 text,
    row2 integer,
    CONSTRAINT table1_pkey PRIMARY KEY (KEY)
);
COMMENT ON COLUMN public.table1.row1 IS 'Строковое значение';
COMMENT ON TABLE  public.table1      IS 'Тестовая таблица 1';

CREATE TABLE table2 (
    key uuid NOT NULL,
    row1 text,
    row2 integer,
    row3 uuid,
    CONSTRAINT table2_pkey PRIMARY KEY (KEY),
    CONSTRAINT table2_fkey FOREIGN KEY (row3) REFERENCES public.table1 (KEY)
);
COMMENT ON COLUMN public.table2.row1 IS 'Строковое значение';
COMMENT ON TABLE  public.table2      IS 'Тестовая таблица 2';


CREATE VIEW view_test1 AS
SELECT key, row2+2 as sum FROM table1;



SELECT * FROM  meta.schema;

SELECT 
    entity,
    title,
    primarykey,
    base_entity,
    table_type,
    hint,
--    view_definition,
    entity_id,
    base_entity_key,
    base_entity_id                                  FROM meta.entity;
SELECT *                                            FROM meta.property; 
SELECT *                                            FROM meta.relation; 
SELECT *                                            FROM meta.projection_entity; 
SELECT *                                            FROM meta.projection_property;
SELECT *                                            FROM meta.projection_relation; 

SELECT * FROM  meta.entity_type;
SELECT * FROM  meta.property_type; 
SELECT * FROM  meta.page_block_layout;

SELECT * FROM  meta.menu;

SELECT * FROM  meta.property_add;
SELECT * FROM  meta.projection_relation_ex;
SELECT * FROM  meta.user_list;

SELECT * FROM  meta.view_entity;
SELECT * FROM  meta.view_projection_entity;
SELECT * FROM  meta.view_projection_property;
SELECT * FROM  meta.view_projection_relation;
SELECT * FROM  meta.view_projection_buttons;

SELECT * FROM  meta.view_page;
 
 
 
 