-- complain IF script is sourced IN psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pg_abris" to load this file. \quit

CREATE SCHEMA meta;


--
--
--  extra
--
--
CREATE TABLE meta.entity_extra (
    entity_id       OID   NOT NULL -- Ключевое поле
  , primarykey      TEXT           -- (ТОЛЬКО ДЛЯ ПРЕДСТАВЛЕНИЙ !!!) Необходимо для хранения ключа в представления
  , base_entity_id  OID            -- (ТОЛЬКО ДЛЯ ПРЕДСТАВЛЕНИЙ !!!) Указывает таблицу к которой будут добавлены поля представления 
  , e_schema        TEXT           -- Схема (для переноса между базами) 
  , e_table         TEXT           -- Таблица (для переноса между базами)
  , b_schema        TEXT           -- Базовая схема (для переноса между базами)
  , b_table         TEXT           -- Базовая таблица (для переноса между базами)
  , CONSTRAINT entity_extra_pkey PRIMARY KEY (entity_id)
);
--
--
CREATE FUNCTION meta.entity_ex_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF new.entity_id IS NULL THEN
    SELECT v.oid 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE n.nspname = new.e_schema AND v.relname = new.e_table 
      INTO new.entity_id;
  ELSE
    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.entity_id 
      INTO new.e_schema, new.e_table;

    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.base_entity_id 
      INTO new.b_schema, new.b_table;
  END IF;
  RETURN new;
END;$$;
--
--
CREATE TRIGGER entity_ex_trg BEFORE INSERT OR UPDATE  
ON meta.entity_extra FOR EACH ROW EXECUTE PROCEDURE meta.entity_ex_trgf();
--
--
--  extra
--
--
CREATE TABLE meta.property_extra (
    property_name   TEXT NOT NULL  -- Ключевое поле
  , type            TEXT           -- Тип при отображении в проекции (есть пометка ref)   
  , ref_entity      OID            -- (ТОЛЬКО ДЛЯ ПРЕДСТАВЛЕНИЙ !!!) Необходимо для хранения зависимостей в представлениях
  , ref_key         TEXT           -- (ТОЛЬКО ДЛЯ ПРЕДСТАВЛЕНИЙ !!!) Необходимо для хранения зависимостей в представлениях
  , e_schema        TEXT           -- Схема (для переноса между базами) 
  , e_table         TEXT           -- Таблица (для переноса между базами)
  , p_name          TEXT           -- Свойство (для переноса между базами) 
  , r_schema        TEXT           -- Базовая схема (для переноса между базами)
  , r_table         TEXT           -- Базовая таблица (для переноса между базами)
  , CONSTRAINT property_extra_pkey PRIMARY KEY ( property_name)
);
--
--
CREATE FUNCTION meta.property_ex_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF new.property_name IS NULL THEN
    SELECT v.oid || '_' || new.p_name 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE n.nspname = new.e_schema AND v.relname = new.e_table 
      INTO new.entity_id;
  ELSE
    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = substring(new.property_name, '(\d+)_')::oid   
      INTO new.e_schema, new.e_table;

    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.ref_entity 
      INTO new.r_schema, new.r_table;
    new.p_name = substring(new.property_name, '\d+_(.+)');
  END IF;
  RETURN new;
END;$$;
--
--
CREATE TRIGGER property_ex_trg BEFORE INSERT OR UPDATE  
ON meta.property_extra FOR EACH ROW EXECUTE PROCEDURE meta.property_ex_trgf();
--
--
--  extra
--
--
CREATE TABLE meta.relation_extra (
    relation_name   TEXT NOT NULL  -- Ключевое поле
  , entity_id       OID            -- Базовая таблица              
  , relation_entity OID            -- Подсоединяемая таблица 
  , key             TEXT           -- Поле в подсоединяемой таблице которое указывает на ключевое поле базовай таблицы 
  , title           TEXT           -- Заголовок 
  , e_schema        TEXT           -- Схема (для переноса между базами) 
  , e_table         TEXT           -- Таблица (для переноса между базами)
  , r_schema        TEXT           -- Базовая схема (для переноса между базами)
  , r_table         TEXT           -- Базовая таблица (для переноса между базами)
  , CONSTRAINT relation_extra_pkey PRIMARY KEY (relation_name)
);
--
--
CREATE FUNCTION meta.relation_ex_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  relation_name TEXT;
BEGIN
  IF new.relation_name IS NULL THEN
    SELECT v.oid  
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE n.nspname = new.e_schema AND v.relname = new.e_table 
      INTO relation_name;
    SELECT relation_name||'_'||v.oid  
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE n.nspname = new.e_schema AND v.relname = new.e_table 
      INTO new.relation_name;
  ELSE
    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.entity_id   
      INTO new.e_schema, new.e_table;
    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.relation_entity 
      INTO new.r_schema, new.r_table;
  END IF;
  RETURN new;
END;$$;
--
--
CREATE TRIGGER relation_ex_trg BEFORE INSERT OR UPDATE  
ON meta.relation_extra FOR EACH ROW EXECUTE PROCEDURE meta.relation_ex_trgf();
--
--
--  extra
--
--
CREATE TABLE meta.projection_extra (
    projection_name TEXT NOT NULL           -- Ключевое поле
  , title           TEXT
  , jump            TEXT
  , additional      TEXT                    -- дополнительные параметры 
  , readonly        BOOLEAN DEFAULT false
  , entity_id       OID NOT NULL
  , hint            TEXT
  , e_schema        TEXT                    -- Схема (для переноса между базами) 
  , e_table         TEXT                    -- Таблица (для переноса между базами)
  , CONSTRAINT projection_entity_extra_pkey PRIMARY KEY (projection_name, entity_id)
);
--
--
CREATE FUNCTION meta.projection_ex_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF new.entity_id IS NULL THEN
    SELECT v.oid 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE n.nspname = new.e_schema AND v.relname = new.e_table 
      INTO new.entity_id;
  ELSE
    SELECT n.nspname, v.relname 
      FROM pg_class v LEFT JOIN pg_namespace n ON n.oid = v.relnamespace 
      WHERE v.oid = new.entity_id 
      INTO new.e_schema, new.e_table;
  END IF;
  RETURN new;
END;$$;
--
--
CREATE TRIGGER projection_ex_trg BEFORE INSERT OR UPDATE  
ON meta.projection_extra FOR EACH ROW EXECUTE PROCEDURE meta.projection_ex_trgf();
--
--
--  extra
--
--
CREATE TABLE meta.projection_property_extra (
    column_name               TEXT NOT NULL
  , title                     TEXT
  , type                      TEXT
  , readonly                  BOOLEAN
  , visible                   BOOLEAN
  , projection_name           TEXT NOT NULL
  , ref_projection            TEXT
  , "order"                   INTEGER
  , concat_prev               BOOLEAN
  , projection_property_name  TEXT
  , hint                      TEXT
  , pattern                   TEXT
  , CONSTRAINT projection_property_extra_pkey PRIMARY KEY (projection_name, column_name)
);

CREATE TABLE meta.projection_relation_extra (
    title TEXT,
    readonly BOOLEAN,
    visible BOOLEAN,
    projection_name TEXT NOT NULL,
    related_projection_name TEXT,
    opened BOOLEAN DEFAULT true,
    "order" INTEGER,
    view_id TEXT,
    relation_entity TEXT,
    projection_relation_name TEXT NOT NULL,
    hint TEXT,
    CONSTRAINT projection_relation_extra_pkey PRIMARY KEY (projection_relation_name)
);

CREATE TABLE meta.projection_buttons (
    button TEXT NOT NULL,
    projection_name TEXT NOT NULL,
    title TEXT,
    icon TEXT,
    function TEXT,
    schema TEXT,
    use_in_list BOOLEAN,
    CONSTRAINT projection_buttons_pkey PRIMARY KEY (button, projection_name)
);


CREATE TABLE meta.menu_item (
    name TEXT NOT NULL,
    title TEXT,
    parent TEXT,
    projection TEXT,
    view_id TEXT DEFAULT 'list',
    role TEXT,
    "order" INTEGER DEFAULT 0,
    iconclass TEXT,
    style TEXT,
    key TEXT,
    CONSTRAINT menu_item_pkey PRIMARY KEY (name)
);

CREATE TABLE meta.page (
    page_key TEXT NOT NULL,
    title TEXT,
    CONSTRAINT page_pkey PRIMARY KEY (page_key)
);


CREATE TABLE meta.page_block (
    page_key TEXT,
    block_key INTEGER  NOT NULL,
    size_percent INTEGER,
    parent_block_key INTEGER,
    view_id TEXT,
    projection_name TEXT,
    entity_id TEXT,
    "order" INTEGER,
    layout INTEGER,
    CONSTRAINT page_block_pkey PRIMARY KEY (block_key)
);

--------------------------------
CREATE TABLE meta.pivot (
    entity_id TEXT NOT NULL,
    entity_row TEXT,
    title_column TEXT,
    num_column TEXT,
    hint_column TEXT,
    CONSTRAINT pivot_pkey PRIMARY KEY (entity_id)
);


CREATE FUNCTION meta.clean() 
  RETURNS TEXT
  LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM meta.entity_extra WHERE entity_id NOT IN (SELECT entity_id FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity_id NOT IN (SELECT entity_id FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity_id||'.'||column_name NOT IN (SELECT entity||'.'||column_name FROM meta.property);
  DELETE FROM meta.relation_extra WHERE relation_name NOT IN (SELECT relation_name FROM meta.relation);
  DELETE FROM meta.projection_extra WHERE entity_id NOT IN (SELECT entity_id FROM meta.entity);
  DELETE FROM meta.projection_property_extra WHERE projection_property_name NOT IN (SELECT projection_property_name FROM meta.projection_property);
  DELETE FROM meta.projection_relation_extra WHERE projection_relation_name NOT IN (SELECT projection_relation_name FROM meta.projection_relation);
  DELETE FROM meta.menu_item WHERE projection NOT IN (SELECT projection_name FROM meta.projection);
  RETURN 'Метаданные успешно очищены';
END;$$;
--
--
--  entity
--
--
CREATE VIEW  meta.entity AS 
  SELECT 
    v.oid                                       AS entity_id,
    n.nspname                                   AS schema_name,
    v.relname                                   AS table_name,
    COALESCE( obj_description(v.oid), 
                v.relname)                      AS title,
    COALESCE(ee.primarykey, b.base_entity_key)  AS primarykey,
    v.relkind::TEXT                             AS table_type,
    pg_get_viewdef(v.oid)::TEXT                 AS view_definition,
    b.base_entity_key                           AS base_entity_key,
    b.base_entity_id                            AS base_entity_id
  FROM pg_class v
    LEFT JOIN pg_namespace n ON n.oid = v.relnamespace
    LEFT JOIN 
    ( SELECT 
        t.refobjid        AS entity_id,
        t.obj             AS base_entity_id,
        at.attname::TEXT  AS base_entity_key
      FROM 
        ( SELECT  
            dv.refobjid,
            min(dt.refobjid) AS obj
          FROM pg_depend dv
            JOIN pg_depend dt ON  dv.objid = dt.objid AND 
                                  dv.refobjid <> dt.refobjid AND 
                                  dt.classid = 'pg_rewrite'::regclass::oid AND 
                                  dt.refclassid = 'pg_class'::regclass::oid
          WHERE dv.refclassid = 'pg_class'::regclass::oid AND dv.classid = 'pg_rewrite'::regclass::oid AND dv.deptype = 'i'::"char"
          GROUP BY dv.refobjid
        ) t
        JOIN pg_class n_1 ON n_1.oid = t.obj AND n_1.relkind = 'r'::"char"
        LEFT JOIN pg_constraint c ON c.conrelid = n_1.oid AND c.contype = 'p'::"char"
        LEFT JOIN pg_attribute at ON c.conkey[1] = at.attnum AND at.attrelid = c.conrelid
        LEFT JOIN pg_namespace ns ON ns.oid = n_1.relnamespace
    ) b ON v.oid = b.entity_id
    LEFT JOIN meta.entity_extra ee ON ee.entity_id = v.oid
  WHERE 
    v.relkind = ANY (ARRAY['v'::"char"])
    AND (pg_has_role(v.relowner, 'USAGE'::TEXT) 
      OR has_table_privilege(v.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER') 
      OR has_any_column_privilege(v.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
    ) 
    AND n.nspname <> ALL (ARRAY['pg_catalog', 'information_schema', 'meta'])
UNION ALL
  SELECT 
    r.oid                                       AS entity_id,
    n.nspname                                   AS schema_name,
    r.relname                                   AS table_name,
    COALESCE(obj_description(r.oid), r.relname) AS title,
    at.attname                                  AS primarykey,
    r.relkind::TEXT                             AS table_type,
    NULL::TEXT                                  AS view_definition,
    NULL::name                                  AS base_entity_key,
    NULL::oid                                   AS base_entity_id
  FROM pg_class r
    LEFT JOIN pg_namespace n ON n.oid = r.relnamespace
    LEFT JOIN pg_constraint c ON c.conrelid = r.oid AND c.contype = 'p'::"char"
    LEFT JOIN pg_attribute at ON c.conkey[1] = at.attnum AND at.attrelid = c.conrelid
  WHERE 
    r.relkind = ANY (ARRAY['r'::"char"]) 
    AND (pg_has_role(r.relowner, 'USAGE') 
      OR has_table_privilege(r.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER') 
      OR has_any_column_privilege(r.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
    ) 
    AND (n.nspname <> ALL (ARRAY['pg_catalog', 'information_schema', 'meta']));
--
--
--  property
--
--
CREATE VIEW meta.property AS 
  SELECT 
    c.oid ||'_'|| a.attname                                         AS property_name,
    a.attname::TEXT                                                 AS column_name,
    c.oid                                                           AS entity_id,
    COALESCE(
        CASE
            WHEN co.conkey[1] IS NOT NULL THEN 'ref'
            WHEN a.atttypid = 2950::oid THEN 'invisible'
            ELSE NULL::TEXT
        END, 'string')                                              AS type,
    CASE
      WHEN t.typtype = 'd' THEN
        CASE
            WHEN t.typelem <> 0 AND t.typlen = '-1' THEN 'ARRAY'
            WHEN nt.nspname = 'pg_catalog'          THEN format_type(a.atttypid, NULL)
                                                    ELSE 'USER-DEFINED'
        END
        ELSE
        CASE
            WHEN t.typelem <> 0 AND t.typlen = '-1' THEN 'ARRAY'
            WHEN nt.nspname = 'pg_catalog'          THEN format_type(a.atttypid, NULL)
                                                    ELSE 'USER-DEFINED'
        END ||
        CASE
            WHEN a.atttypmod = '-1'                   THEN ''
            WHEN a.atttypid = ANY (ARRAY[1042, 1043]) THEN '('||a.atttypmod-4||')'
            WHEN a.atttypid = ANY (ARRAY[1560, 1562]) THEN '('||a.atttypmod|| ')'
                                                      ELSE ''
        END
      END::information_schema.character_data                        AS data_type,
    true                                                            AS visible,
    CASE
      WHEN (c.relkind = ANY (ARRAY['f', 'p'])) 
        OR (c.relkind = ANY (ARRAY['v', 'r'])) 
        AND NOT pg_column_is_updatable(c.oid::regclass, a.attnum, false) 
        THEN true
        ELSE false
    END                                                             AS readonly,
    COALESCE(d.description, a.attname::TEXT)                        AS title,
    COALESCE(pe.ref_entity, r.oid)                                  AS ref_entity,
    COALESCE(pe.ref_key, at.attname::TEXT)::TEXT                    AS ref_key,
    a.attnum * 10                                                   AS "order",
    co.conname::information_schema.sql_identifier                   AS constraint_name,
    NOT (a.attnotnull OR t.typtype = 'd'::"char" AND t.typnotnull)  AS is_nullable,
    pg_get_expr(ad.adbin, ad.adrelid)                               AS "default"
  FROM pg_attribute a
    LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
    JOIN (pg_class c
      JOIN pg_namespace nc ON c.relnamespace = nc.oid) ON a.attrelid = c.oid
      JOIN (pg_type t
        JOIN pg_namespace nt ON t.typnamespace = nt.oid) ON a.atttypid = t.oid
    LEFT JOIN meta.property_extra pe ON property_name  = c.oid ||'.'|| a.attname
    LEFT JOIN (pg_constraint co
    JOIN (pg_class r
      LEFT JOIN pg_namespace nr ON r.relnamespace = nr.oid
    JOIN (pg_constraint cr
    JOIN pg_attribute at ON cr.conkey[1] = at.attnum AND at.attrelid = cr.conrelid) 
      ON r.oid = cr.conrelid AND cr.contype = 'p'::"char") 
      ON r.oid = co.confrelid) 
      ON c.oid = co.conrelid AND co.contype = 'f'::"char" AND a.attnum = co.conkey[1]
    LEFT JOIN pg_description d ON a.attnum = d.objsubid AND a.attrelid = d.objoid
  WHERE a.attnum > 0 
    AND NOT a.attisdropped 
    AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char", 'p'::"char"])) 
    AND (pg_has_role(c.relowner, 'USAGE'::TEXT) 
      OR has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::TEXT)) 
    AND (nc.nspname <> ALL (ARRAY['information_schema'::name, 'pg_catalog'::name, 'meta'::name]));
--
--
--  property_add
--
--
CREATE VIEW meta.property_add AS
  SELECT 
    entity.base_entity_id||'.~'||entity.entity_id||'.~'||a.attname  AS property_name,
    entity.base_entity_id   AS entity_id,
    false                         AS visible,
    true                          AS readonly,
    'string'                      AS type,
    a.attname::TEXT               AS title,
    ( CASE
        WHEN t.typtype = 'd' THEN
          CASE
            WHEN t.typelem <> 0 AND t.typlen = '-1' THEN 'ARRAY'
            WHEN nt.nspname = 'pg_catalog'          THEN format_type(a.atttypid, NULL)
                                                    ELSE 'USER-DEFINED'
          END
          ELSE (
            CASE
              WHEN ((t.typelem <> 0) AND (t.typlen = '-1')) THEN 'ARRAY'
              WHEN (nt.nspname = 'pg_catalog')              THEN format_type(a.atttypid, NULL)
              ELSE                                               'USER-DEFINED'
            END ||
            CASE
              WHEN a.atttypmod = '-1'                   THEN ''
              WHEN a.atttypid = ANY (ARRAY[1042, 1043]) THEN '(' || a.atttypmod - 4 || ')'
              WHEN a.atttypid = ANY (ARRAY[1560, 1562]) THEN '(' || a.atttypmod || ')'
                                                        ELSE ''
          END)
      END
    )::TEXT                       AS data_type,
    entity.entity_id              AS ref_entity,
    '~' || a.attname              AS column_name,
    entity.base_entity_key::TEXT  AS ref_key,
    a.attnum * 10 + 1000          AS "order",
    true                          AS virtual,
    (a.attname)::TEXT             AS original_column_name,
    false                         AS is_nullable,
    NULL::TEXT                    AS "default"
  FROM meta.entity
    JOIN pg_attribute a ON a.attrelid = entity.entity_id
    JOIN pg_type t
      JOIN pg_namespace nt ON t.typnamespace = nt.oid ON a.atttypid = t.oid
  WHERE a.attnum > 0 AND (NOT a.attisdropped) AND entity.base_entity_id IS NOT NULL AND entity.primarykey <> a.attname 
UNION
  SELECT property.property_name,
    property.entity_id,
    property.visible,
    property.readonly,
    property.type,
    property.title,
    property.data_type,
    property.ref_entity,
    property.column_name,
    property.ref_key,
    property."order",
    NULL::BOOLEAN AS virtual,
    NULL AS original_column_name,
    property.is_nullable,
    property."default"
   FROM meta.property;
--
--
--  relation
--
--
CREATE VIEW meta.relation AS
  SELECT 
    r.oid||'_'||e.oid                         AS relation_name,
    e.oid                                     AS relation_entity,
    r.oid                                     AS entity_id,
    COALESCE(
      obj_description(c.oid, 'pg_constraint')
      ,e.relname)                             AS title,
    at.attname                                AS key,
    false                                     AS virtual
  FROM pg_class e
    JOIN      pg_constraint       c   ON e.oid = c.conrelid AND c.contype = 'f'::"char"
    LEFT JOIN pg_class            r   ON r.oid = c.confrelid
    LEFT JOIN pg_attribute        at  ON  c.conkey[1] = at.attnum AND at.attrelid = c.conrelid
UNION
  SELECT 
    re.relation_name,
    re.relation_entity,
    re.entity_id,
    re.title,
    re.key,
    true                            AS virtual
  FROM meta.relation_extra re;
--
--
--  projection
--
--
CREATE VIEW meta.projection AS
  SELECT 
    COALESCE(pe.projection_name, e.table_name)                         AS projection_name,
    e.entity_id                                                        AS entity_id,
    false                                                              AS base,
    COALESCE(pe.title, e.title)                                        AS title,
    COALESCE(pe.jump, COALESCE(pe.projection_name, e.table_name))      AS jump,
    e.primarykey                                                       AS primarykey,
    pe.additional                                                      AS additional,
    COALESCE(pe.readonly,
     (NOT has_table_privilege(e.entity_id, 'INSERT, UPDATE, DELETE'))) AS readonly,
    e.base_entity_id                                                   AS base_entity_id,
    pe.hint                                                            AS hint
  FROM meta.entity e
     LEFT JOIN meta.projection_extra pe ON pe.entity_id = e.entity_id
  WHERE (EXISTS ( SELECT pe1.projection_name FROM meta.projection_extra pe1
                    WHERE (pe1.entity_id = e.entity_id)))
  UNION
  SELECT 
    e.table_name      AS projection_name,
    e.entity_id       AS entity_id,
    true              AS base,
    e.title           AS title,
    e.table_name      AS jump,
    e.primarykey      AS primarykey,
    NULL::TEXT        AS additional,
    (NOT has_table_privilege(e.entity_id, 'insert, update, DELETE'::TEXT)) AS readonly,
    e.base_entity_id  AS base_entity_id,
    null              AS hint
   FROM meta.entity e;
--
--
--  projection_property
--
--
CREATE VIEW meta.projection_property AS
  SELECT 
    projection.projection_name||'.'|| property.column_name  AS projection_property_name,
    COALESCE( projection_property_extra.title, 
              property.title)                               AS title,
    COALESCE( projection_property_extra.type, 
              property.type)                                AS type,
    COALESCE( projection_property_extra.readonly, 
              property.readonly)                            AS readonly,
    COALESCE( projection_property_extra.visible, 
              property.visible)                             AS visible,
    projection.projection_name                              AS projection_name,
    property.column_name                                    AS column_name,
    property.ref_key                                        AS ref_key,
    COALESCE( projection_property_extra.ref_projection,
              pr.projection_name)                           AS ref_projection,
    NULL                                                    AS link_key,
    COALESCE( projection_property_extra."order", 
              property."order")                             AS "order",
    property.ref_entity,
    NULL                                                    AS ref_filter,
    COALESCE( projection_property_extra.concat_prev, 
              false)                                        AS concat_prev,
    property.virtual                                        AS virtual,
    property.original_column_name                           AS original_column_name,
    projection_property_extra.hint                          AS hint,
    projection_property_extra.pattern                       AS pattern,
    property.is_nullable                                    AS is_nullable,
    property."default"                                      AS "default"
  FROM meta.projection
    LEFT JOIN meta.property_add property ON projection.entity_id = property.entity_id
    LEFT JOIN meta.projection_property_extra ON property.column_name = projection_property_extra.column_name 
       AND projection.projection_name = projection_property_extra.projection_name
    LEFT JOIN meta.projection pr ON property.ref_entity = pr.entity_id AND pr.base = true;
--
--
--  projection_relation
--
--
CREATE VIEW meta.projection_relation AS
  SELECT 
    p.projection_name||'.'||relation.relation_name AS projection_relation_name,
    COALESCE(pre.title, relation.title)            AS title,
    COALESCE( pre.related_projection_name, 
              entity.table_name)                   AS related_projection_name,
    COALESCE(pre.readonly, false)                  AS readonly,
    COALESCE(pre.visible, true)                    AS visible,
    p.projection_name                              AS projection_name,
    relation.relation_entity                       AS relation_entity,
    relation.entity_id                             AS entity_id,
    relation.key                                   AS key,
    COALESCE(pre.opened, false)                    AS opened,
    pre."order"                                    AS "order",
    pre.view_id                                    AS view_id,
    pre.hint                                       AS hint
  FROM        meta.projection p
    JOIN      meta.relation                       ON relation.entity_id = p.entity_id
    LEFT JOIN meta.entity                         ON relation.relation_entity = entity.entity_id 
    LEFT JOIN meta.projection_relation_extra  pre ON 
        p.projection_name||'.'||relation.relation_name = pre.projection_relation_name;
--
--
--  
--
--
CREATE VIEW meta.functions AS
 SELECT p.proname AS function_name,
    p.prosrc AS function_code,
    n.nspname AS function_schema,
    (((n.nspname)::TEXT || '.'::TEXT) || (p.proname)::TEXT) AS function_key
   FROM (pg_namespace n
     JOIN pg_proc p ON ((p.pronamespace = n.oid)))
  WHERE n.nspname <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name, 'meta'::name]);
--
--
--  
--
--
CREATE VIEW meta.menu AS
  WITH RECURSIVE temp1(name, parent, title, projection, view_id, role, path, level, iconclass) AS (
    SELECT 
      t1.name,
      t1.parent,
      t1.title,
      t1.projection,
      t1.view_id,
      t1.role,
      (to_char(t1."order", '000'::TEXT) || t1.name) AS path,
      1,
      t1.iconclass,
      t1.style,
      t1.key
    FROM meta.menu_item t1
      WHERE (t1.parent IS NULL)
    UNION
    SELECT 
      t2.name,
      t2.parent,
      t2.title,
      t2.projection,
      t2.view_id,
      t2.role,
      (((temp1_1.path || '->'::TEXT) || to_char(t2."order", '000'::TEXT)) || t2.name) AS "varchar",
      (temp1_1.level + 1),
      t2.iconclass,
      t2.style,
      t2.key
    FROM (meta.menu_item t2
      JOIN temp1 temp1_1 ON ((temp1_1.name = t2.parent)))
  )
 SELECT temp1.name,
    temp1.parent,
    temp1.title,
    temp1.projection,
    temp1.view_id,
    temp1.role,
    temp1.iconclass,
    temp1.path,
    temp1.style,
    temp1.key
   FROM temp1
  WHERE ((( SELECT count(tin.name) AS count
           FROM (temp1 tin
             JOIN meta.projection ON ((tin.projection = projection.projection_name)))
          WHERE (tin.parent = temp1.name)) > 0) AND ((temp1.role IS NULL) OR pg_has_role("current_user"(), (temp1.role)::name, 'member'::TEXT)))
UNION
 SELECT temp1.name,
    temp1.parent,
    temp1.title,
    temp1.projection,
    temp1.view_id,
    temp1.role,
    temp1.iconclass,
    temp1.path,
    temp1.style,
    temp1.key
   FROM (temp1
     JOIN meta.projection ON ((temp1.projection = projection.projection_name)))
  WHERE ((temp1.role IS NULL) OR pg_has_role("current_user"(), (temp1.role)::name, 'member'::TEXT))
 LIMIT 1000;
--
--
--  
--
--
CREATE TABLE meta.page_block_layout (
    layout  INTEGER NOT NULL
  , name    TEXT
  , CONSTRAINT page_block_layout_pkey PRIMARY KEY (layout)
);
COMMENT ON TABLE  meta.page_block_layout        IS 'Способы рамположения блоков';
COMMENT ON COLUMN meta.page_block_layout.layout IS 'Тип расположения';
COMMENT ON COLUMN meta.page_block_layout.name   IS 'Расположение';
--
--
INSERT INTO meta.page_block_layout VALUES (0, 'Нет'                     ); 
INSERT INTO meta.page_block_layout VALUES (1, 'Вертикальная раскладка'  );
INSERT INTO meta.page_block_layout VALUES (2, 'Горизонтальная раскладка');
--
--
--  
--
--
CREATE TABLE meta.entity_type (
    type  CHARACTER
  , note  TEXT
  , CONSTRAINT entity_type_pkey PRIMARY KEY (type)
);
COMMENT ON TABLE  meta.entity_type      IS 'Типы сущьностей';
COMMENT ON COLUMN meta.entity_type.type IS 'Тип';
COMMENT ON COLUMN meta.entity_type.note IS 'Наименование';
--
--
INSERT INTO meta.entity_type VALUES ('r', 'Таблица');
INSERT INTO meta.entity_type VALUES ('v', 'Представление');
--
--
--  
--
--
CREATE TABLE meta.property_type (
    type    TEXT
  , note    TEXT
  , CONSTRAINT property_type_pkey PRIMARY KEY (type)
);
COMMENT ON TABLE  meta.property_type      IS 'Типы свойст';
COMMENT ON COLUMN meta.property_type.type IS 'Тип';
COMMENT ON COLUMN meta.property_type.note IS 'Наименование';
--
--
INSERT INTO meta.property_type VALUES ('bool'      , 'Истина или ложь'                      );
INSERT INTO meta.property_type VALUES ('button'    , 'Кнопка'                               );
INSERT INTO meta.property_type VALUES ('caption'   , 'Заголовок'                            );
INSERT INTO meta.property_type VALUES ('date'      , 'Дата'                                 );
INSERT INTO meta.property_type VALUES ('datetime'  , 'Дата и время'                         );
INSERT INTO meta.property_type VALUES ('file'      , 'Файл'                                 );
INSERT INTO meta.property_type VALUES ('INTEGER'   , 'Целочисленное'                        );
INSERT INTO meta.property_type VALUES ('address'   , 'Адрес'                                );
INSERT INTO meta.property_type VALUES ('plain'     , 'Текст без форматирования'             );
INSERT INTO meta.property_type VALUES ('ref'       , 'Список'                               );
INSERT INTO meta.property_type VALUES ('ref_link'  , 'Ссылка (обычная)'                     );
INSERT INTO meta.property_type VALUES ('string'    , 'Строковые значения'                   );
INSERT INTO meta.property_type VALUES ('TEXT'      , 'Форматированный текст'                );
INSERT INTO meta.property_type VALUES ('time'      , 'Время'                                );
INSERT INTO meta.property_type VALUES ('titleLink' , 'Ссылка с названием (ссылка||название)');
INSERT INTO meta.property_type VALUES ('money'     , 'Денежный'                             );
INSERT INTO meta.property_type VALUES ('ref_tree'  , 'Ссылка на классификатор'              );
INSERT INTO meta.property_type VALUES ('parent_id' , 'Ссылка на родителя'                   );
INSERT INTO meta.property_type VALUES ('row_color' , 'Цвет строки'                          );
INSERT INTO meta.property_type VALUES ('filedb'    , 'Файл в базе'                          );
INSERT INTO meta.property_type VALUES ('progress'  , 'Горизонтальный индикатор'             );
INSERT INTO meta.property_type VALUES ('invisible' , 'Скрытый'                              );;
--
--
--  
--
--
CREATE VIEW meta.schema AS
  SELECT schemata.schema_name
     FROM information_schema.schemata
  WHERE ((schemata.schema_name)::TEXT <> ALL (ARRAY[('pg_toast'::character varying)::TEXT, ('pg_temp_1'::character varying)::TEXT, ('pg_toast_temp_1'::character varying)::TEXT, ('pg_catalog'::character varying)::TEXT, ('information_schema'::character varying)::TEXT]));
--
--
--  
--
--
CREATE VIEW meta.view_entity AS
  SELECT 
    entity.entity_id,
    entity.title,
    entity.primarykey,
    entity.base_entity_id,
    entity.table_type
   FROM meta.entity
  WHERE entity.schema_name <> ALL (ARRAY['ems'::TEXT, 'meta'::TEXT]);
--
--
--  
--
--
CREATE VIEW meta.view_page AS
 SELECT page.page_key,
    page.title
   FROM meta.page;
--
--
--  
--
--
CREATE VIEW meta.view_page_block AS
 WITH RECURSIVE temp1(page_key, block_key, size_percent, parent_block, view_id, projection_name, entity_id, "order", layout, path, level) AS (
         SELECT t1.page_key,
            t1.block_key,
            t1.size_percent,
            t1.parent_block_key,
            t1.view_id,
            t1.projection_name,
            t1.entity_id,
            t1."order",
            t1.layout,
            COALESCE(to_char(t1."order", '000'::TEXT), '000'::TEXT) AS path,
            1
           FROM meta.page_block t1
          WHERE (t1.parent_block_key IS NULL)
        UNION
         SELECT t2.page_key,
            t2.block_key,
            t2.size_percent,
            t2.parent_block_key,
            t2.view_id,
            t2.projection_name,
            t2.entity_id,
            t2."order",
            t2.layout,
            ((temp1_1.path || '->'::TEXT) || COALESCE(to_char(t2."order", '000'::TEXT), '000'::TEXT)),
            (temp1_1.level + 1)
           FROM (meta.page_block t2
             JOIN temp1 temp1_1 ON ((temp1_1.block_key = t2.parent_block_key)))
        )
 SELECT temp1.page_key,
    temp1.block_key,
    temp1.size_percent,
    temp1.parent_block,
    temp1.view_id,
    temp1.projection_name,
    temp1.entity_id,
    temp1."order",
    temp1.layout,
    temp1.path,
    temp1.level
   FROM temp1
  ORDER BY temp1.path
 LIMIT 1000;
--
--
--  
--
--
CREATE VIEW meta.view_projection_buttons AS
 SELECT projection_buttons.button,
    projection_buttons.projection_name,
    projection_buttons.title,
    projection_buttons.icon,
    projection_buttons.function,
    projection_buttons.schema,
    projection_buttons.use_in_list
   FROM meta.projection_buttons;
--
--
--  
--
--
CREATE VIEW meta.view_projection_entity AS
 SELECT projection.projection_name,
    -- projection.table_name,
    -- projection.table_schema,
    projection.title,
    projection.jump,
    projection.primarykey,
    projection.additional,
    projection.readonly,
    projection.hint
   FROM meta.projection;
--
--
--  
--
--
CREATE VIEW meta.view_projection_property AS
 SELECT projection_property.projection_property_name,
    projection_property.title,
    projection_property.type,
    projection_property.readonly,
    projection_property.visible,
    projection_property.projection_name,
    projection_property.column_name,
    projection_property.ref_key,
    projection_property.ref_projection,
    projection_property.link_key,
    projection_property."order",
    projection_property.ref_entity,
    projection_property.ref_filter,
    projection_property.concat_prev,
    projection_property.virtual,
    projection_property.original_column_name,
    projection_property.hint,
    projection_property.pattern,
    projection_property.is_nullable,
    projection_property."default"
   FROM meta.projection_property;
--
--
--  
--
--
CREATE VIEW meta.view_projection_relation AS
 SELECT projection_relation.projection_relation_name,
    projection_relation.title,
    projection_relation.related_projection_name,
    projection_relation.readonly,
    projection_relation.visible,
    projection_relation.projection_name,
    projection_relation.key,
    projection_relation.opened,
    projection_relation.view_id,
    projection_relation.hint,
    projection_relation."order"
   FROM meta.projection_relation;
--
--
--  
--
--
CREATE FUNCTION meta.entity_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  DECLARE
    name_key            TEXT;
    new_schema_name     TEXT;
    new_table_name      TEXT;
    new_entity          TEXT;
  BEGIN
    IF  TG_OP = 'DELETE' THEN 
      IF old.table_type = 'VIEW' THEN
        EXECUTE('DROP VIEW '||old.schema_name||'.'||old.table_name||';');
      ELSE
        EXECUTE('DROP TABLE '||old.schema_name||'.'||old.table_name||';');
      END IF;
      PERFORM  meta.clean();  
      RETURN old;
    END IF;
    IF  TG_OP = 'INSERT' THEN 
      IF new.schema_name IS NULL THEN
        new_schema_name = 'public';
      ELSE 
        new_schema_name = quote_ident(lower(new.schema_name));
        IF new_schema_name SIMILAR TO '[a-z][a-z,_,0-9]{0,62}' THEN
          IF NOT EXISTS(SELECT schema_name FROM information_schema.schemata WHERE schema_name = new_schema_name) THEN
            RAISE EXCEPTION 'Схемы - % не существует.', new_schema_name;
            RETURN new;
          END IF;
        ELSE
          RAISE EXCEPTION 'Некорректное имя схемы - %.', new_schema_name;
          RETURN new;
        END IF;
      END IF;     
      new_table_name  = quote_ident(lower(new.table_name)); 
      IF new_table_name SIMILAR TO '[a-z][a-z,_,0-9]{0,62}' THEN
        new_entity = new_schema_name||'.'||new_table_name;
      ELSE
        RAISE EXCEPTION 'Некорректное имя таблицы - %.', new_table_name;
        RETURN new;
      END IF;
      IF (new.view_definition is NOT null) THEN -- добавление представления
        EXECUTE ( 'CREATE VIEW '||new_entity||' AS ' || new.view_definition );
        EXECUTE ( 'COMMENT ON VIEW  '||new_entity||' IS '''|| new.title||'''');
      ELSE                                      -- добавление таблицы
        IF new.primarykey IS NULL THEN
          name_key = quote_ident(new.tablename||'_key');
        ELSE
          name_key = quote_ident(new.primarykey);  
        END IF;
        EXECUTE('CREATE TABLE '||new_entity||
          ' ("'||name_key||'" uuid default uuid_generate_v4(), CONSTRAINT "'||
          new_table_name||'_pkey"  PRIMARY KEY ("'||name_key||
          '"))'
        );
        EXECUTE('COMMENT ON TABLE  '||new_entity||' IS '''|| new.title||'''');
      END IF;
    RETURN new;
  END IF;
  IF  TG_OP = 'UPDATE' THEN 
    UPDATE meta.entity_extra SET 
      base_entity_id = new.base_entity_id,
      primarykey = new.primarykey
    WHERE entity_extra.entity_id = new.entity_id;

    IF old.table_type = 'v' THEN
      IF new.view_definition <> old.view_definition THEN
        EXECUTE ( 'CREATE OR REPLACE VIEW '||new.schema_name||'.'||new.table_name||' AS ' || new.view_definition );
      END IF;
      IF new.title <> old.title THEN
        EXECUTE ( 'COMMENT ON VIEW  '||new.schema_name||'.'||new.table_name||' IS '''|| new.title||'''');
      END IF;
    ELSE  
      IF new.title <> old.title THEN
        EXECUTE ( 'COMMENT ON TABLE  '||new.schema_name||'.'||new.table_name||' IS '''|| new.title||'''');
      END IF;
    END IF;
    INSERT INTO meta.entity_extra(
      entity_id, 
      base_entity_id, 
      primarykey) 
      SELECT 
        new.entity_id, 
        new.base_entity_id,
        new.primarykey
    WHERE NOT exists (SELECT * FROM  meta.entity_extra WHERE entity_extra.entity_id = new.entity_id);
    RETURN new;
  END IF;
END;$$;
--
--
CREATE TRIGGER entity_trg INSTEAD OF INSERT OR UPDATE OR DELETE 
ON meta.entity FOR EACH ROW EXECUTE PROCEDURE meta.entity_trgf();
--
--
--  
--
--
CREATE FUNCTION meta.property_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS 
$$DECLARE
  old_entity          TEXT;
  old_constraint_name TEXT;
  old_column_name     TEXT;
  new_entity          TEXT;
  new_column_name     TEXT;
  new_ref_entity      TEXT;
  new_ref_key         TEXT;
  new_data_type       TEXT;
  new_property_name   TEXT;

BEGIN
  IF  TG_OP = 'DELETE' THEN 
    SELECT quote_ident(schema_name)||'.'||quote_ident(table_name) AS entity FROM meta.entity WHERE entity_id = old.entity_id  INTO old_entity;
    EXECUTE ('ALTER TABLE '||old_entity||' DROP COLUMN ' || quote_ident(old.column_name)); 
    DELETE FROM meta.property_extra WHERE property_name = old.entity_id ||'_'|| old.column_name;
    RETURN old;
  END IF;   

  IF  TG_OP = 'INSERT' THEN 
    SELECT quote_ident(schema_name)||'.'||quote_ident(table_name) AS entity FROM meta.entity WHERE entity_id = new.entity_id  INTO new_entity;  
    SELECT quote_ident(schema_name)||'.'||quote_ident(table_name) AS entity FROM meta.entity WHERE entity_id = new.ref_entity INTO new_ref_entity;  
    IF new.ref_entity IS NOT NULL THEN -- добавление ссылочного поля
      SELECT primarykey FROM meta.entity WHERE entity_id = new.ref_entity INTO new.ref_key;
      new_ref_key = quote_ident( new.ref_key );
      IF new.column_name IS NULL THEN
        new_column_name = quote_ident( new.ref_key );
      ELSE  
        new_column_name = quote_ident( new.column_name );
      END IF;

      SELECT data_type FROM meta.property WHERE entity_id = new.ref_entity and column_name = new.ref_key INTO new.data_type;
      new_data_type := quote_ident(new.data_type);
      new.type = 'ref';
      EXECUTE('ALTER TABLE '||new_entity||' add column "'||new_column_name||'" '||new_data_type||';');
      EXECUTE('ALTER TABLE '||new_entity||' 
        ADD CONSTRAINT '||substring(new_entity, '\.(\w*)')||'_'||new_column_name||'_fkey FOREIGN KEY ("'||new_column_name||'")
        REFERENCES '||new_ref_entity||' ('||new_ref_key||') MATCH SIMPLE
        ON UPDATE NO ACTION ON DELETE NO ACTION;');
    ELSE -- добавление обычного поля
      IF new.data_type IS NULL THEN
        new.data_type = 'TEXT';
      END IF;
      EXECUTE 'ALTER TABLE '||new_entity||' add column '||new.column_name||'  '||new.data_type||'';
    END IF;
    IF new.title IS NOT NULL THEN -- Сохранение названия колонки
      EXECUTE 'COMMENT ON COLUMN ' || new_entity || '.' ||new.column_name|| ' IS ''' ||  new.title || '''';
    END IF;
    INSERT INTO meta.property_extra(
      property_name,
      ref_key, 
      ref_entity) 
      SELECT 
        new.entity_id ||'_'|| COALESCE(new_column_name, new.column_name), 
        new.ref_key, 
        new.ref_entity;
    RETURN new;
  END IF;

  IF  TG_OP = 'UPDATE' THEN 
    SELECT quote_ident(schema_name)||'.'||quote_ident(table_name) AS entity FROM meta.entity WHERE entity_id = old.entity_id  INTO old_entity;  
    SELECT quote_ident(schema_name)||'.'||quote_ident(table_name) AS entity FROM meta.entity WHERE entity_id = new.entity_id  INTO new_entity;  
  
    old_constraint_name := quote_ident(old.constraint_name);
    new_property_name = new.entity_id || '.' || new.column_name;

    IF EXISTS ( SELECT * FROM meta.entity WHERE entity_id = new.entity_id AND table_type = 'r' ) THEN
      IF old.ref_entity IS NOT NULL AND (old.ref_entity<>new.ref_entity OR new.ref_entity IS NULL) THEN
        IF (old.constraint_name IS NOT NULL) THEN
          EXECUTE('ALTER TABLE '||old_entity||' DROP CONSTRAINT  "'||old_constraint_name||'"');
          new.ref_key = NULL;
        END IF;
      END IF;
      IF new.ref_entity IS NOT NULL AND (old.ref_entity<>new.ref_entity OR old.ref_entity IS NULL) THEN
        SELECT primarykey FROM meta.entity WHERE entity = new.ref_entity INTO new_ref_key;
        new_column_name := quote_ident(new.column_name);
        new_ref_entity := quote_ident(meta.entity_to_schema(new.ref_entity)) || '.' || quote_ident(meta.entity_to_table(new.ref_entity::TEXT));
        new_entity := meta.entity_to_table(new.entity);
        EXECUTE('ALTER TABLE '||old_entity||' ADD CONSTRAINT "'||new_entity||'_'||new.column_name||'_fkey" FOREIGN KEY ("'||new.column_name||'")
          REFERENCES '||new_ref_entity||' ("'||new_ref_key||'") MATCH SIMPLE
          ON UPDATE NO ACTION ON DELETE NO ACTION;');
      END IF;
    END IF;
    IF new.data_type <> old.data_type THEN
      old_column_name = quote_ident(old.column_name);
      new_data_type = quote_ident(new.data_type);
      EXECUTE 'alter table '||old_entity||' alter column '||old_column_name||' type '||new_data_type||' using ('||old_column_name||'::'||new_data_type||')';
    END IF;
    IF new.title <> old.title THEN -- Сохранение названия колонки
      EXECUTE 'COMMENT ON COLUMN ' || new_entity || '.' ||new.column_name|| ' IS ''' ||  new.title || '''';
    END IF;
    UPDATE meta.property_extra set 
        ref_key    = new.ref_key, 
        ref_entity = new.ref_entity
      WHERE property_extra.property_name = new_property_name;
    INSERT INTO meta.property_extra(
      property_name,
      ref_key, 
      ref_entity) 
      SELECT 
        new_property_name,
        new.ref_key, 
        new.ref_entity
      WHERE NOT EXISTS
        (SELECT * FROM  meta.property_extra WHERE property_extra.property_name = new_property_name);
    RETURN new;   
  END IF;      
END;$$;
--
--
CREATE TRIGGER property_trg INSTEAD OF INSERT OR UPDATE OR DELETE 
  ON meta.property FOR EACH ROW EXECUTE PROCEDURE meta.property_trgf();
--
--
--  
--
--
CREATE FUNCTION meta.relation_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF  TG_OP = 'DELETE' THEN 
    IF old.virtual = FALSE THEN
      EXECUTE(
        ' ALTER TABLE '||(select schema_name||'.'||table_name from meta.entity where entity_id = old.relation_entity)||
        ' DROP CONSTRAINT  "'||(select conname from pg_constraint where conrelid = old.relation_entity and confrelid = old.entity_id)||'"');
    ELSE
      DELETE FROM meta.relation_extra WHERE relation_name = old.relation_name;  
    END IF;
    RETURN old;
  END IF;

  IF (select count(*) from meta.property where entity_id = new.relation_entity and column_name = new.key) <> 1 THEN
    RAISE EXCEPTION 'Неверное имя ключа - %', new.key;
  END IF;

  IF  TG_OP = 'UPDATE' THEN 
    IF new.virtual <> old.virtual THEN
      RAISE EXCEPTION 'Тип зависимости поменять нельзя.';
    END IF;
    IF new.title <> old.title AND new.virtual = false THEN
      EXECUTE ( 
        'COMMENT ON CONSTRAINT  '||(select conname from pg_constraint where conrelid = new.relation_entity and confrelid = new.entity_id)||
        ' ON '||(select schema_name||'.'||table_name from meta.entity where entity_id = new.relation_entity) ||
        ' IS ''' ||new.title||'''');
	  END IF;
    IF new.virtual = true THEN
      UPDATE meta.relation_extra SET
          relation_entity = new.relation_entity,
          entity_id = new.entity_id,
          title = new.title,
          key = new.key
        WHERE relation_extra.relation_name = new.entity_id||'_'||new.relation_entity;
	  END IF;
    RETURN new;   
  END IF;

  IF  TG_OP = 'INSERT' THEN 
    IF new.virtual = FALSE THEN
      -- EXECUTE(
      --   'ALTER TABLE '||new_entity||' 
      --   ADD CONSTRAINT '||substring(new_entity, '\.(\w*)')||'_'||new_column_name||'_fkey
      --   FOREIGN KEY ("'||new_column_name||'")
      --   REFERENCES '||new_ref_entity||' ('||new_ref_key||') 
      --   MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION;');
      RAISE EXCEPTION 'Вставка зависимости только через колонки';
    ELSE
      INSERT INTO meta.relation_extra(
          relation_name,
          relation_entity,
          entity_id,
          title,
          key
        ) 
        SELECT
          new.entity_id||'_'||new.relation_entity as relation_name,
          new.relation_entity,
          new.entity_id,
          new.title,
          new.key
        WHERE NOT exists
        (SELECT * FROM  meta.relation_extra WHERE relation_extra.relation_name = new.entity_id||'_'||new.relation_entity);
    END IF;
    RETURN new;   
  END IF;
  RETURN new;   
END;$$;
--
--
CREATE TRIGGER relation_trg INSTEAD OF INSERT OR UPDATE OR DELETE
  ON meta.relation FOR EACH ROW 
  EXECUTE PROCEDURE meta.relation_trgf();
--
--
--  
--
--
CREATE FUNCTION meta.projection_trgf() RETURNS trigger
  LANGUAGE plpgsql
  AS $$
BEGIN
  IF  TG_OP = 'DELETE' THEN 
    DELETE FROM meta.projection_extra WHERE projection_name = old.projection_name;
    RETURN old;
  END IF;
  IF  TG_OP = 'INSERT' THEN
    INSERT INTO meta.projection_extra(projection_name, entity_id, title, additional, readonly, jump) 
      SELECT new.projection_name, new.entity_id, new.title, new.additional, new.readonly, new.jump;
    RETURN new;
  END IF;
  IF  TG_OP = 'UPDATE' THEN
    UPDATE meta.projection_extra SET 
        title = new.title, 
        jump = new.jump, 
        additional = new.additional, 
        readonly = new.readonly, 
        hint = new.hint 
      WHERE projection_extra.projection_name = new.projection_name;
    INSERT INTO meta.projection_extra(projection_name, entity_id, title , jump, additional, readonly, hint) 
      SELECT new.projection_name, new.entity, new.title, new.jump, new.additional, new.readonly, new.hint WHERE NOT exists
      (SELECT * FROM  meta.projection_extra WHERE projection_extra.projection_name = new.projection_name);
    RETURN new;
  END IF;
END;$$;
--
--
CREATE TRIGGER projection_trg INSTEAD OF INSERT OR UPDATE OR DELETE 
  ON meta.projection FOR EACH ROW EXECUTE PROCEDURE meta.projection_trgf();
--
--
--  
--
--
CREATE FUNCTION meta.projection_property_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp TEXT;
BEGIN
 UPDATE meta.projection_property_extra set 
        title = new.title, 
  visible = new.visible, 
  readonly = new.readonly, 
  type = new.type,
  ref_projection = new.ref_projection,
  "order" = new."order",
  concat_prev = new.concat_prev,
  hint = new.hint
 WHERE projection_property_extra.projection_property_name = new.projection_property_name RETURNING projection_property_name INTO _temp;

--UPDATE meta.property set title = new.title,
--  visible = new.visible,
--  readonly = new.readonly,
--  type = new.type,
--  ref_key = new.ref_key,
--  link_key = new.link_key,
--  ref_entity = new.ref_entity,
--  "order" = new."order",
--  hint = new.hint
-- WHERE column_name = new.column_name and entity IN (SELECT substring(entity, '\w*')||'.'||projection_name FROM meta.projection WHERE projection_name = old.projection_name);

 IF _temp IS NULL THEN
 INSERT INTO meta.projection_property_extra(
    projection_property_name, 
    projection_name, 
    column_name, 
    title, 
    visible, 
    readonly, 
    type, 
    ref_projection, 
    "order",  
    concat_prev,
    hint) 
    SELECT new.projection_property_name,
           new.projection_name,
     new.column_name,
     new.title, 
           new.visible, 
     new.readonly, 
     new.type,
     new.ref_projection,
     new."order",
     new.concat_prev,
     new.hint
--    WHERE NOT exists
--   (SELECT * FROM  meta.projection_property_extra WHERE projection_property_extra.projection_property_name = new.projection_property_name);
 ;
 END IF;
 RETURN new;      
END;$$;



CREATE FUNCTION meta.projection_relation_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp_title TEXT;
BEGIN
--  UPDATE meta.projection_relation_extra set 
--     title = new.title, 
--     visible = new.visible, 
--     readonly = new.readonly, 
--     related_projection_name = new.related_projection_name,
--     opened = new.opened,
--     view_id = new.view_id,
--     "order" = new.order,
--     hint = new.hint
--   WHERE projection_relation_extra.projection_relation_name = new.projection_relation_name RETURNING title INTO _temp_title;
  
--  --UPDATE meta.relation set
--  --   title = new.title,
--  --   entity = new.entity,
--  --   "order" = new."order"
--  -- WHERE relation.relation_entity = new.relation_entity and relation.entity = new.projection;
-- IF _temp_title IS NULL THEN 
--  INSERT INTO meta.projection_relation_extra(
--    projection_relation_name,
--    projection_name, 
--    relation_entity, 
--    title, 
--    visible, 
--    readonly, 
--    related_projection_name, 
--    "order", 
--    view_id,
--    hint) 
--     SELECT 
--        new.projection_relation_name,
--       new.projection_name, 
--       new.relation_entity, 
--       new.title, 
--       new.visible, 
--       new.readonly, 
--       new.related_projection_name, 
--       new.order, 
--       new.view_id,
--       new.hint 
--     WHERE NOT exists
--    (SELECT * FROM  meta.projection_relation_extra 
--       WHERE projection_relation_extra.projection_relation_name = new.projection_name||'.'||new.entity||'.'||new.relation_entity||'.'||new.key);
-- END IF;
 RETURN new;   
END;$$;












CREATE FUNCTION meta.function_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN

  PERFORM udf_dropfunction(old.function_name);
  RETURN old;
END;$$;


CREATE FUNCTION meta.function_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$DECLARE
  new_function_schema TEXT;
  new_function_name TEXT;
  new_function_code TEXT;

BEGIN
  new_function_schema := quote_ident(new.function_schema);
  new_function_name := quote_ident(new.function_name);
  new_function_code := quote_ident(new.function_code);

  EXECUTE '
CREATE OR REPLACE FUNCTION ' || new.function_schema || '.' || new.function_name || ' (row_key uuid, user_key uuid) RETURNS TEXT AS $BODY$' || new.function_code || '$BODY$ LANGUAGE plpgsql VOLATILE NOT LEAKPROOF';
  RETURN new;
END;$_$;



CREATE FUNCTION meta.function_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$DECLARE
  new_function_schema TEXT;
  new_function_name TEXT;
  new_function_code TEXT;

BEGIN
  new_function_schema := quote_ident(new.function_schema);
  new_function_name := quote_ident(new.function_name);
  new_function_code := quote_ident(new.function_code);

  PERFORM udf_dropfunction(old.function_name);
  EXECUTE '
CREATE OR REPLACE FUNCTION ' || new.function_schema || '.' || new.function_name || ' (row_key uuid, user_key uuid) RETURNS TEXT AS $BODY$' || new.function_code || '$BODY$ LANGUAGE plpgsql VOLATILE NOT LEAKPROOF';
  RETURN new;
END;$_$;




CREATE FUNCTION meta.grants_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_entity_id TEXT;
  new_groname TEXT;

BEGIN
  -- new_entity := quote_ident(new.entity);
  -- new_groname := quote_ident(new.groname);

  --  IF new.INSERT <> old.INSERT THEN

  --    IF new.INSERT = true THEN
  --      execute ('GRANT INSERT ON TABLE '||new.entity||' TO '||new.groname);
  --    ELSE
  --      execute ('REVOKE INSERT ON TABLE '||new.entity||' FROM '||new.groname);
  --    END IF;   
  --  END IF;


  --  IF new.update <> old.update THEN
  --    IF new.update = true THEN
  --      execute ('GRANT UPDATE ON TABLE '||new.entity||' TO '||new.groname);
  --    ELSE
  --      execute ('REVOKE UPDATE ON TABLE '||new.entity||' FROM '||new.groname);
  --    END IF;   
  --  END IF;


  --  IF new.DELETE <> old.DELETE THEN
  --    IF new.DELETE = true THEN
  --      execute ('GRANT DELETE ON TABLE '||new.entity||' TO '||new.groname);
  --    ELSE
  --      execute ('REVOKE DELETE ON TABLE '||new.entity||' FROM '||new.groname);
  --    END IF;   
  --  END IF;


  --  IF new.SELECT <> old.SELECT THEN
  --    IF new.SELECT = true THEN
  --      execute ('GRANT SELECT ON TABLE '||new.entity||' TO '||new.groname);
  --    ELSE
  --      execute ('REVOKE SELECT ON TABLE '||new.entity||' FROM '||new.groname);
  --    END IF;   
  --  END IF;
   
   RETURN new;
END;$$;



CREATE FUNCTION meta.menu_item_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
 
 IF new.name is null THEN
  new.name = new.projection;
 END IF;
 
 IF new.title is null THEN
   new.title  = coalesce((SELECT projection.title FROM meta.projection WHERE projection.projection_name = new.projection), new.projection);
 END IF;

 RETURN new;
END;$$;




CREATE INDEX fki_menu_item_fk ON meta.menu_item USING btree (parent);

CREATE TRIGGER function_delete_trg INSTEAD OF DELETE ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_delete_trgf();

CREATE TRIGGER function_insert_trg INSTEAD OF INSERT ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_insert_trgf();

CREATE TRIGGER function_update_trg INSTEAD OF UPDATE ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_update_trgf();

--CREATE TRIGGER grants_update_trg INSTEAD OF UPDATE ON meta.grants FOR EACH ROW EXECUTE PROCEDURE meta.grants_update_trgf();

CREATE TRIGGER menu_item_tr BEFORE INSERT ON meta.menu_item FOR EACH ROW EXECUTE PROCEDURE meta.menu_item_insert_trgf();

CREATE TRIGGER projection_property_update_trg INSTEAD OF INSERT OR UPDATE ON meta.projection_property FOR EACH ROW EXECUTE PROCEDURE meta.projection_property_update_trgf();

--
--
CREATE TRIGGER projection_relation_update_trg INSTEAD OF INSERT OR UPDATE 
  ON meta.projection_relation FOR EACH ROW 
  EXECUTE PROCEDURE meta.projection_relation_update_trgf();



   
   

