CREATE EXTENSION pg_abris CASCADE;

--
--  Базовые таблицы для просмотра значений по умолчанию
--
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
COMMENT ON CONSTRAINT table2_fkey ON public.table2   IS 'Зависимость';

CREATE VIEW view_test1 AS
SELECT key, row2+2 as sum FROM table1;

SELECT * FROM meta.entity;
SELECT * FROM meta.property; 
SELECT * FROM meta.relation; 
--
--
--  Проверка entity
--
--
--
--  Вставка новой таблицы
--
INSERT INTO meta.entity (schema_name, table_name, title, primarykey) VALUES('public', 'test_Insert', 'Таблица через insert', 'id');
SELECT * FROM meta.entity;
--
--  Вставка нового представления
--
INSERT INTO meta.entity (schema_name, table_name, title, view_definition) VALUES('public', 'view_insert', 'Представление через insert',
    'select * from public.table2');
SELECT * FROM meta.entity;
--
--  Обновление представления
--
UPDATE meta.entity SET view_definition = 'select *, 1 from public.table2' WHERE  table_name = 'view_insert';
SELECT * FROM meta.entity;
--
--
--  Проверка property
--
--
--
--  Вставка колонки
--
INSERT INTO meta.property (entity_id, column_name, data_type)  
    VALUES((select entity_id from meta.entity where table_name = 'table1'), 'note', 'integer');
INSERT INTO meta.property (entity_id, column_name, title)  
    VALUES((select entity_id from meta.entity where table_name = 'table1'), 'name', 'Наименование');
INSERT INTO meta.property (entity_id, ref_entity)   VALUES((select entity_id from meta.entity where table_name = 'test_insert'),
                                                           (select entity_id from meta.entity where table_name = 'table1'));
SELECT * FROM meta.property; 
--
--  Изменение колонки
--
UPDATE meta.property SET title = 'Комментарий' WHERE property_name = (select entity_id from meta.entity where table_name = 'table1')||'_'|| 'note';
UPDATE meta.property SET data_type  = 'text' WHERE property_name = (select entity_id from meta.entity where table_name = 'table1')||'_'|| 'note';
SELECT * FROM meta.property; 
--
--
--  Проверка relation
--
--
--
--  Вставка виртуальной новой зависимости 
--
INSERT INTO meta.relation (entity_id, relation_entity, key, virtual, title )  
    VALUES((select entity_id from meta.entity where table_name = 'table1'),
           (select entity_id from meta.entity where table_name = 'test_insert'),  'key', true, 'Заголовок');
SELECT * FROM meta.relation; 
--
--  Обновление   зависимости 
--
UPDATE meta.relation SET title = 'Зависимость после редактирования' 
    WHERE relation_name = (select entity_id from meta.entity where table_name = 'table1')||'_'||  
     (select entity_id from meta.entity where table_name = 'table2');
SELECT * FROM meta.relation; 
--
--
--  Проверка schema
--
--
--
SELECT * FROM  meta.schema;
--
--
--  Проверка проекций
--
--
--
SELECT * FROM meta.projection; 
SELECT * FROM meta.projection_property;
SELECT * FROM meta.projection_relation; 


INSERT INTO meta.projection (projection_name, entity_id, title ) 
    VALUES ('table1_edit',  (select entity_id from meta.entity where table_name = 'table1'), 'Для редактирования');
INSERT INTO meta.projection (projection_name, entity_id, title ) 
    VALUES ('table1_add',  (select entity_id from meta.entity where table_name = 'table1'), 'Для дополнения');
SELECT * FROM meta.projection; 

--
--
--  Проверка справочников
--
--
--
SELECT * FROM  meta.entity_type;
SELECT * FROM  meta.property_type; 
SELECT * FROM  meta.page_block_layout;
--
--
--  Проверка menu
--
--
--
SELECT * FROM meta.entity_extra;
SELECT * FROM meta.property_extra;
SELECT * FROM meta.entity_extra;
SELECT * FROM meta.relation_extra;
--
--
--  Проверка menu
--
--
--
SELECT * FROM  meta.menu;
--
--
--  Проверка всего остального
--
--
SELECT * FROM  meta.property_add;
--
--
--  Проверка view
--
--
SELECT * FROM  meta.view_entity;
SELECT * FROM  meta.view_projection_entity;
SELECT * FROM  meta.view_projection_property;
SELECT * FROM  meta.view_projection_relation;
SELECT * FROM  meta.view_projection_buttons;
SELECT * FROM  meta.view_page;
 
 
DELETE FROM meta.relation  
    WHERE relation_name = (select entity_id from meta.entity where table_name = 'table1')||'_'||  
     (select entity_id from meta.entity where table_name = 'table2');

DELETE FROM meta.relation  
    WHERE relation_name = (select entity_id from meta.entity where table_name = 'table1')||'_'||  
     (select entity_id from meta.entity where table_name = 'test_insert');
--
--
SELECT * FROM meta.relation;
-- 
--
--
-- 
DELETE FROM meta.property  WHERE property_name = (select entity_id from meta.entity where table_name = 'table1')||'.'|| 'note';
--
--
SELECT * FROM meta.property;
