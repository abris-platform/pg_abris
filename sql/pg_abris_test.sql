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


INSERT INTO meta.entity (schema_name, table_name, title, primarykey) VALUES('public', 'test_Insert', 'Таблица через insert', 'id');

INSERT INTO meta.property (entity_id, column_name)  VALUES((select entity_id from meta.entity where table_name = 'table1'), 'note');
INSERT INTO meta.property (entity_id, ref_entity)   VALUES((select entity_id from meta.entity where table_name = 'test_insert'),
                                                           (select entity_id from meta.entity where table_name = 'table1'));

UPDATE meta.property SET title = 'Комментарий' WHERE property_name = (select entity_id from meta.entity where table_name = 'table1')||'.'|| 'note';

INSERT INTO meta.entity (schema_name, table_name, title, view_definition) VALUES('public', 'view_insert', 'Представление через insert',
    'select * from public.table2');


UPDATE meta.entity SET view_definition = 'select *, 1 from public.table2' WHERE  table_name = 'view_insert';



SELECT * FROM  meta.schema;

SELECT * FROM meta.entity;
SELECT * FROM meta.property; 
SELECT * FROM meta.relation; 
SELECT * FROM meta.projection_entity; 
SELECT * FROM meta.projection_property;
SELECT * FROM meta.projection_relation; 

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
 
 
 
 