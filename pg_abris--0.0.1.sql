-- complain IF script is sourced IN psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pg_abris" to load this file. \quit



CREATE SCHEMA meta;

CREATE TABLE meta.entity_extra (
    title text NOT NULL,
    primarykey text,
    entity text NOT NULL,
    base_entity text NOT NULL,
    hint text
);

CREATE TABLE meta.relation_add (
    relation_name text NOT NULL,
    relation_entity oid,
    entity oid,
    title text,
    key character varying,
    "order" integer,
    hint text
);

CREATE TABLE meta.relation_extra (
    title text,
    ref_key text,
    relation_name text NOT NULL,
    "order" integer,
    hint text
);

CREATE TABLE meta.projection_entity_extra (
    title text,
    projection_name text NOT NULL,
    jump text,
    additional text,
    readonly boolean DEFAULT false,
    entity oid NOT NULL,
    hint text
);

CREATE TABLE meta.projection_relation_extra (
    title text,
    readonly boolean,
    visible boolean,
    projection_name text NOT NULL,
    related_projection_name text,
    opened boolean DEFAULT true,
    "order" integer,
    view_id text,
    relation_entity text,
    projection_relation_name text NOT NULL,
    hint text
);

CREATE TABLE meta.menu_item (
    name text NOT NULL,
    title text,
    parent text,
    projection text,
    view_id text DEFAULT 'list'::text,
    role text,
    "order" integer DEFAULT 0,
    iconclass text,
    style text,
    key text
);

CREATE TABLE meta.page (
    page_key text NOT NULL,
    title text
);

CREATE TABLE meta.page_block (
    page_key text,
    block_key uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    size_percent integer,
    parent_block_key uuid,
    view_id text,
    projection_name text,
    entity_id text,
    "order" integer,
    layout integer
);



CREATE TABLE meta.pivot (
    entity text NOT NULL,
    entity_row text,
    title_column text,
    num_column text,
    hint_column text
);

CREATE TABLE meta.projection_buttons (
    button text NOT NULL,
    projection_name text NOT NULL,
    title text,
    icon text,
    function text,
    schema text,
    use_in_list boolean
);

CREATE TABLE meta.projection_property_extra (
    column_name text NOT NULL,
    title text,
    type text,
    readonly boolean,
    visible boolean,
    projection_name text NOT NULL,
    ref_projection text,
    "order" integer,
    concat_prev boolean,
    projection_property_name text,
    hint text
);

CREATE TABLE meta.property_extra (
    column_name text NOT NULL,
    title text,
    type text,
    readonly boolean,
    visible boolean,
    ref_entity text,
    ref_filter text,
    ref_key text,
    link_key text,
    "order" integer,
    entity text NOT NULL,
    hint text,
    pattern text
);

CREATE TABLE meta.projection_redirect (
    projection_name text,
    projection_redirect_to text,
    role text,
    projection_redirect text NOT NULL
);


CREATE FUNCTION meta.entity_to_table(
  parameter text) 
  RETURNS text
  LANGUAGE plpgsql
AS $$
DECLARE
  result text;
BEGIN
  result := substring(parameter, '\.(\w*)'::text);
  RETURN result;
END;
$$;



CREATE FUNCTION meta.entity_to_schema(
  parameter text) 
  RETURNS text
  LANGUAGE plpgsql
AS $$
DECLARE
  result text;
BEGIN
  result := substring(parameter, '\w*'::text);
  RETURN result;
END;
$$;


CREATE FUNCTION meta.get_name_column(
  entity text, 
  "column" text) 
  RETURNS text
  LANGUAGE plpgsql
AS $$
DECLARE
  r  text;
  e   text;
  t text;
  n text;
BEGIN
  execute ('SELECT title_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO t;
  execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO n;
  execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO e;
  execute ('SELECT '||t||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') INTO r;
  RETURN r; 
END;$$;



CREATE FUNCTION meta.get_hint_column(
  entity text,
  "column" text) 
  RETURNS text
  LANGUAGE plpgsql
AS $$
DECLARE
  r  text;
  e   text;
  t text;
  n text;
BEGIN
  execute ('SELECT hint_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO t;
  execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO n;
  execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO e;
  execute ('SELECT '||t||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') INTO r;
  RETURN r; 
END;$$;



CREATE FUNCTION meta.get_order_column(
 entity text,
 "column" text) 
 RETURNS integer
 LANGUAGE plpgsql
AS $$
DECLARE
  r  integer;
  e   text;
  t text;
  n text;
BEGIN
  execute ('SELECT hint_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO t;
  execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO n;
  execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') INTO e;
  execute ('SELECT '||n||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') INTO r;
  RETURN 100+r; 
END;$$;


CREATE FUNCTION meta.clean() 
  RETURNS text
  LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM meta.entity_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity||'.'||column_name NOT IN (SELECT entity||'.'||column_name FROM meta.property);
  DELETE FROM meta.relation_extra WHERE relation_name NOT IN (SELECT relation_name FROM meta.relation);
  DELETE FROM meta.projection_entity_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.projection_property_extra WHERE projection_property_name NOT IN (SELECT projection_property_name FROM meta.projection_property);
  DELETE FROM meta.projection_relation_extra WHERE projection_relation_name NOT IN (SELECT projection_relation_name FROM meta.projection_relation);
  DELETE FROM meta.menu_item WHERE projection NOT IN (SELECT projection_name FROM meta.projection_entity);
  RETURN 'Метаданные успешно очищены';
END;$$;
--
--
--  entity
--
--
CREATE VIEW  meta.entity AS 
  SELECT 
    n.nspname||'.'||v.relname                                 AS entity,
    COALESCE(ee.title, v.relname)                             AS title,
    COALESCE(ee.primarykey, b.base_entity_key)                AS primarykey,
    COALESCE(ee.base_entity, b.base_entity)                   AS base_entity,
    v.relkind::information_schema.character_data              AS table_type,
    ee.hint                                                   AS hint,
    pg_get_viewdef(v.oid)::information_schema.character_data  AS view_definition,
    v.oid                                                     AS entity_id,
    b.base_entity_key,
    b.base_entity_id
  FROM pg_class v
    LEFT JOIN pg_namespace n ON n.oid = v.relnamespace
    LEFT JOIN 
    ( SELECT 
        t.refobjid        AS entity_id,
        t.obj             AS base_entity_id,
        n_1.oid::text     AS base_entity,
        at.attname::text  AS base_entity_key
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
    LEFT JOIN meta.entity_extra ee ON ee.entity = v.oid::text
  WHERE 
    v.relkind = ANY (ARRAY['v'::"char"])
    AND (pg_has_role(v.relowner, 'USAGE'::text) 
      OR has_table_privilege(v.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER') 
      OR has_any_column_privilege(v.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
    ) 
    AND n.nspname <> ALL (ARRAY['pg_catalog', 'information_schema', 'meta'])
UNION ALL
  SELECT 
    n.nspname||'.'||r.relname                                       AS entity,
    COALESCE(ee.title, COALESCE(obj_description(r.oid), r.relname)) AS title,
    at.attname                                                      AS primarykey,
    null                                                            AS base_entity,
    r.relkind::text                                                 AS table_type,
    ee.hint                                                         AS hint,
    NULL::text                                                      AS view_definition,
    r.oid                                                           AS entity_id,
    NULL::name                                                      AS base_entity_key,
    NULL::oid                                                       AS base_entity_id
  FROM pg_class r
    LEFT JOIN pg_namespace n ON n.oid = r.relnamespace
    LEFT JOIN pg_constraint c ON c.conrelid = r.oid AND c.contype = 'p'::"char"
    LEFT JOIN pg_attribute at ON c.conkey[1] = at.attnum AND at.attrelid = c.conrelid
    LEFT JOIN meta.entity_extra ee ON ee.entity = r.oid::text
  WHERE 
    r.relkind = ANY (ARRAY['r'::"char"]) 
    AND (pg_has_role(r.relowner, 'USAGE') 
      OR has_table_privilege(r.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER') 
      OR has_any_column_privilege(r.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
    ) 
    AND (n.nspname <> ALL (ARRAY['pg_catalog', 'information_schema', 'meta']));
--
--
--  projection_entity
--
--
CREATE VIEW meta.projection_entity AS
  SELECT 
    COALESCE(pee.projection_name, meta.entity_to_table(entity.entity))                                AS projection_name,
    entity.entity_id                                                                                  AS entity,
    COALESCE(pee.title, entity.title)                                                                 AS title,
    COALESCE(pee.jump, COALESCE(pee.projection_name, meta.entity_to_table(entity.base_entity)))       AS jump,
    entity.primarykey                                                                                 AS primarykey,
    pee.additional                                                                                    AS additional,
    COALESCE(pee.readonly, (NOT has_table_privilege(entity.entity, 'INSERT, UPDATE, DELETE'::text)))  AS readonly,
    (0)::bigint                                                                                       AS amount_dup,
    entity.base_entity                                                                                AS base_entity,
    pee.hint                                                                                          AS hint
  FROM (meta.entity
     LEFT JOIN meta.projection_entity_extra pee ON ((pee.entity = entity.entity_id)))
  WHERE (EXISTS ( SELECT pee1.projection_name
           FROM meta.projection_entity_extra pee1
          WHERE (pee1.entity = entity.entity_id)))
UNION
 SELECT meta.entity_to_table(entity.entity)                                                           AS projection_name,
    entity.entity_id                                                                                  AS entity,
    entity.title                                                                                      AS entity,
    meta.entity_to_table(entity.base_entity)                                                          AS jump,
    entity.primarykey                                                                                 AS primarykey,
    NULL                                                                                              AS additional,
    (NOT has_table_privilege(entity.entity, 'INSERT, UPDATE, DELETE'))                                AS readonly,
    (0)::bigint                                                                                       AS amount_dup,
    entity.base_entity                                                                                AS base_entity,
    entity.hint                                                                                       AS hint  
  FROM meta.entity
  WHERE (NOT EXISTS ( SELECT projection_entity_extra.projection_name
           FROM meta.projection_entity_extra
          WHERE (projection_entity_extra.projection_name = meta.entity_to_table(entity.entity))));





CREATE VIEW meta.relation AS
  SELECT 
    r.oid||'.'||e.oid               AS relation_name,
    e.oid                           AS relation_entity,
    r.oid                           AS entity,
    COALESCE(re.title, e.relname)   AS title,
    (at.attname)::character varying AS key,
    COALESCE(re."order", 0)         AS "order",
    re.hint                         AS hint
   FROM pg_class e
     JOIN pg_constraint c ON e.oid = c.conrelid AND c.contype = 'f'::"char"
     LEFT JOIN pg_class r ON r.oid = c.confrelid
     LEFT JOIN pg_attribute at ON c.conkey[1] = at.attnum AND at.attrelid = c.conrelid
     LEFT JOIN meta.relation_extra re ON re.relation_name = r.oid||'.'||e.oid
UNION
  SELECT 
    relation_add.relation_name,
    relation_add.relation_entity,
    relation_add.entity,
    relation_add.title,
    relation_add.key,
    relation_add."order",
    relation_add.hint
  FROM meta.relation_add;

CREATE VIEW meta.projection_relation AS
  SELECT 
    projection_entity.projection_name||'.'||relation.relation_name                              AS projection_relation_name,
    COALESCE(projection_relation_extra.title, relation.title)                                   AS title,
    COALESCE(projection_relation_extra.related_projection_name, relation.relation_entity::text) AS related_projection_name,
    COALESCE(projection_relation_extra.readonly, false)                                         AS readonly,
    COALESCE(projection_relation_extra.visible, true)                                           AS visible,
    projection_entity.projection_name,
    relation.relation_entity,
    relation.entity,
    relation.key,
    COALESCE(projection_relation_extra.opened, false)                                           AS opened,
    COALESCE(projection_relation_extra."order", relation."order")                               AS "order",
    projection_relation_extra.view_id,
    projection_relation_extra.hint
   FROM meta.projection_entity
      JOIN meta.relation ON relation.entity = projection_entity.entity
     LEFT JOIN meta.projection_relation_extra ON 
        projection_entity.projection_name||'.'||relation.relation_name = projection_relation_extra.projection_relation_name
  ORDER BY COALESCE(projection_relation_extra."order", relation."order");



CREATE VIEW meta.entity_type AS
 SELECT 'r'::text AS type,
    'Таблица'::text AS note
UNION
 SELECT 'v'::text AS type,
    'Представление'::text AS note;


CREATE VIEW meta.functions AS
 SELECT p.proname AS function_name,
    p.prosrc AS function_code,
    n.nspname AS function_schema,
    (((n.nspname)::text || '.'::text) || (p.proname)::text) AS function_key
   FROM (pg_namespace n
     JOIN pg_proc p ON ((p.pronamespace = n.oid)))
  WHERE n.nspname <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name, 'meta'::name]);


CREATE VIEW meta.menu AS
  WITH RECURSIVE temp1(name, parent, title, projection, view_id, role, path, level, iconclass) AS (
    SELECT 
      t1.name,
      t1.parent,
      t1.title,
      t1.projection,
      t1.view_id,
      t1.role,
      (to_char(t1."order", '000'::text) || t1.name) AS path,
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
      (((temp1_1.path || '->'::text) || to_char(t2."order", '000'::text)) || t2.name) AS "varchar",
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
             JOIN meta.projection_entity ON ((tin.projection = projection_entity.projection_name)))
          WHERE (tin.parent = temp1.name)) > 0) AND ((temp1.role IS NULL) OR pg_has_role("current_user"(), (temp1.role)::name, 'member'::text)))
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
     JOIN meta.projection_entity ON ((temp1.projection = projection_entity.projection_name)))
  WHERE ((temp1.role IS NULL) OR pg_has_role("current_user"(), (temp1.role)::name, 'member'::text))
 LIMIT 1000;





CREATE VIEW meta.page_block_layout AS
 SELECT 0 AS layout,
    'Нет'::text AS name
UNION
 SELECT 1 AS layout,
    'Вертикальная раскладка'::text AS name
UNION
 SELECT 2 AS layout,
    'Горизонтальная раскладка'::text AS name;





CREATE VIEW meta.property AS 
 SELECT 
    c.oid ||'.'|| a.attname     AS property_name,
    c.oid::text                       AS entity,
    COALESCE(pe.visible, true)  AS visible,
    COALESCE(pe.readonly,
        CASE
            WHEN (c.relkind = ANY (ARRAY['f'::"char", 'p'::"char"])) OR (c.relkind = ANY (ARRAY['v'::"char", 'r'::"char"])) AND NOT pg_column_is_updatable(c.oid::regclass, a.attnum, false) THEN true
            ELSE false
        END) AS readonly,
    COALESCE(
        CASE
            WHEN co.conkey[1] IS NOT NULL THEN 'ref'::text
            WHEN a.atttypid = 2950::oid THEN 'invisible'::text
            ELSE NULL::text
        END, COALESCE(pe.type, 'string'::text)) AS type,
    COALESCE(pe.title, COALESCE(d.description, a.attname::text)) AS title,
        CASE
            WHEN t.typtype = 'd'::"char" THEN
            CASE
                WHEN t.typelem <> 0::oid AND t.typlen = '-1'::integer THEN 'ARRAY'::text
                WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END
            ELSE
            CASE
                WHEN t.typelem <> 0::oid AND t.typlen = '-1'::integer THEN 'ARRAY'::text
                WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END ||
            CASE
                WHEN a.atttypmod = '-1'::integer THEN ''::text
                WHEN a.atttypid = ANY (ARRAY[1042::oid, 1043::oid]) THEN ('('::text || (a.atttypmod - 4)) || ')'::text
                WHEN a.atttypid = ANY (ARRAY[1560::oid, 1562::oid]) THEN ('('::text || a.atttypmod) || ')'::text
                ELSE ''::text
            END
        END::information_schema.character_data AS data_type,
    COALESCE(pe.ref_entity, (nr.nspname::text || '.'::text) || r.relname::text) AS ref_entity,
    a.attname::text AS column_name,
    COALESCE(pe.ref_key, at.attname::information_schema.sql_identifier::text)::information_schema.sql_identifier AS ref_key,
    pe.ref_filter,
    pe.link_key,
    COALESCE(pe."order", a.attnum * 10) AS "order",
    pe.hint,
    co.conname::information_schema.sql_identifier AS constraint_name,
    NOT (a.attnotnull OR t.typtype = 'd'::"char" AND t.typnotnull) AS is_nullable,
    pg_get_expr(ad.adbin, ad.adrelid) AS "default",
    pe.pattern
   FROM pg_attribute a
     LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
     JOIN (pg_class c
     JOIN pg_namespace nc ON c.relnamespace = nc.oid) ON a.attrelid = c.oid
     JOIN (pg_type t
     JOIN pg_namespace nt ON t.typnamespace = nt.oid) ON a.atttypid = t.oid
     LEFT JOIN meta.property_extra pe ON ((nc.nspname::text || '.'::text) || c.relname::text) = pe.entity AND a.attname::text = pe.column_name
     LEFT JOIN (pg_constraint co
     JOIN (pg_class r
     LEFT JOIN pg_namespace nr ON r.relnamespace = nr.oid
     JOIN (pg_constraint cr
     JOIN pg_attribute at ON cr.conkey[1] = at.attnum AND at.attrelid = cr.conrelid) ON r.oid = cr.conrelid AND cr.contype = 'p'::"char") ON r.oid = co.confrelid) ON c.oid = co.conrelid AND co.contype = 'f'::"char" AND a.attnum = co.conkey[1]
     LEFT JOIN pg_description d ON a.attnum = d.objsubid AND a.attrelid = d.objoid
  WHERE a.attnum > 0 AND NOT a.attisdropped AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char", 'p'::"char"])) AND (pg_has_role(c.relowner, 'USAGE'::text) OR has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text)) AND (nc.nspname <> ALL (ARRAY['information_schema'::name, 'pg_catalog'::name, 'meta'::name]));


CREATE VIEW meta.property_add AS
  SELECT 
    entity.base_entity||'.~'||entity.entity||'.~'||a.attname  AS property_name,
    entity.base_entity                                        AS entity,
    false                                                     AS visible,
    true                                                      AS readonly,
    'string'::text                                            AS type,
    (a.attname)::text                                         AS title,
    ( CASE
        WHEN (t.typtype = 'd'::"char") THEN
          CASE
            WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1')) THEN 'ARRAY'
            WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL)
            ELSE 'USER-DEFINED'
          END
          ELSE (
            CASE
              WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1')) THEN 'ARRAY'
              WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL)
              ELSE 'USER-DEFINED'
            END ||
            CASE
              WHEN (a.atttypmod = '-1') THEN ''
              WHEN (a.atttypid = ANY (ARRAY[(1042)::oid, (1043)::oid])) THEN (('(' || (a.atttypmod - 4)) || ')')
              WHEN (a.atttypid = ANY (ARRAY[(1560)::oid, (1562)::oid])) THEN (('(' || a.atttypmod) || ')')
              ELSE ''
          END)
      END
    )::information_schema.character_data                        AS data_type,
    entity.entity                                               AS ref_entity,
    ('~'::text || (a.attname)::text)                            AS column_name,
    (entity.base_entity_key)::information_schema.sql_identifier AS ref_key,
    NULL::text                                                  AS ref_filter,
    NULL::text                                                  AS link_key,
    999                                                         AS "order",
    true                                                        AS virtual,
    (a.attname)::text                                           AS original_column_name,
    NULL::text                                                  AS hint,
    NULL::text                                                  AS pattern,
    false                                                       AS is_nullable,
    NULL::text                                                  AS "default"
   FROM meta.entity
     JOIN pg_attribute a ON a.attrelid = entity.entity_id
     JOIN pg_type t
      JOIN pg_namespace nt ON t.typnamespace = nt.oid ON a.atttypid = t.oid
  WHERE a.attnum > 0 AND (NOT a.attisdropped) AND entity.base_entity_id IS NOT NULL AND entity.primarykey <> a.attname 
UNION
 SELECT property.property_name,
    property.entity,
    property.visible,
    property.readonly,
    property.type,
    property.title,
    property.data_type,
    property.ref_entity,
    property.column_name,
    property.ref_key,
    property.ref_filter,
    property.link_key,
    property."order",
    NULL::boolean AS virtual,
    property.hint AS original_column_name,
    NULL::text AS hint,
    property.pattern,
    property.is_nullable,
    property."default"
   FROM meta.property;



CREATE VIEW meta.projection_property AS
 SELECT ((projection_entity.projection_name || '.'::text) || property.column_name) AS projection_property_name,
    COALESCE(projection_property_extra.title, property.title) AS title,
    COALESCE(projection_property_extra.type, property.type) AS type,
    COALESCE(projection_property_extra.readonly, property.readonly) AS readonly,
    COALESCE(projection_property_extra.visible, property.visible) AS visible,
    projection_entity.projection_name,
    property.column_name,
    property.ref_key,
    COALESCE(projection_property_extra.ref_projection, meta.entity_to_table(property.ref_entity)) AS ref_projection,
    property.link_key,
    COALESCE(projection_property_extra."order", property."order") AS "order",
    property.ref_entity,
    property.ref_filter,
    COALESCE(projection_property_extra.concat_prev, false) AS concat_prev,
    property.virtual,
    property.original_column_name,
    projection_property_extra.hint,
    property.pattern,
    property.is_nullable,
    property."default"
   FROM ((meta.projection_entity
     LEFT JOIN meta.property_add property ON ((projection_entity.entity::text = property.entity::text)))
     LEFT JOIN meta.projection_property_extra ON (((property.column_name = projection_property_extra.column_name) 
       AND (projection_entity.projection_name = projection_property_extra.projection_name))))
  ORDER BY projection_entity.projection_name, COALESCE(projection_property_extra."order", property."order");




CREATE VIEW meta.projection_relation_ex AS
  SELECT 
    projection_entity.projection_name || '.'|| relation.relation_entity                               AS projection_relation_name,
    COALESCE((projection_relation_extra.title || '(*)'), relation.title)                              AS title,
    COALESCE((projection_relation_extra.related_projection_name || '(*)'), relation.relation_entity::text)  AS related_projection_name,
    COALESCE(projection_relation_extra.readonly, false)                                               AS readonly,
    COALESCE(projection_relation_extra.visible, true)                                                 AS visible,
    projection_entity.projection_name,
    relation.relation_entity,
    relation.entity,
    relation.key
  FROM meta.projection_entity
    LEFT JOIN meta.entity ON projection_entity.entity::text = entity.entity
    LEFT JOIN meta.relation ON entity.base_entity::text = relation.entity::text
    LEFT JOIN meta.projection_relation_extra ON relation.relation_entity::text = projection_relation_extra.relation_entity::text 
      AND projection_entity.projection_name = projection_relation_extra.projection_name;



CREATE VIEW meta.property_type AS
 SELECT 'bool'::text AS type,
    'Истина или ложь'::text AS note
UNION
 SELECT 'button'::text AS type,
    'Кнопка'::text AS note
UNION
 SELECT 'caption'::text AS type,
    'Заголовок'::text AS note
UNION
 SELECT 'date'::text AS type,
    'Строка'::text AS note
UNION
 SELECT 'datetime'::text AS type,
    'Дата и время'::text AS note
UNION
 SELECT 'file'::text AS type,
    'Файл'::text AS note
UNION
 SELECT 'getFileForPrint'::text AS type,
    'Файл для печати наряда'::text AS note
UNION
 SELECT 'integer'::text AS type,
    'Целочисленное'::text AS note
UNION
 SELECT 'address'::text AS type,
    'Адрес'::text AS note
UNION
 SELECT 'plain'::text AS type,
    'Текст без форматирования'::text AS note
UNION
 SELECT 'ref'::text AS type,
    'Список'::text AS note
UNION
 SELECT 'ref_link'::text AS type,
    'Ссылка (обычная)'::text AS note
UNION
 SELECT 'string'::text AS type,
    'Строковые значения'::text AS note
UNION
 SELECT 'text'::text AS type,
    'Форматированный текст'::text AS note
UNION
 SELECT 'time'::text AS type,
    'Время'::text AS note
UNION
 SELECT 'titleLink'::text AS type,
    'Ссылка с названием (ссылка||название)'::text AS note
UNION
 SELECT 'breadcrumbs'::text AS type,
    'Хлебные крошки'::text AS note
UNION
 SELECT 'money'::text AS type,
    'Денежный'::text AS note
UNION
 SELECT 'ref_tree'::text AS type,
    'Ссылка на классификатор'::text AS note
UNION
 SELECT 'parent_id'::text AS type,
    'Ссылка на родителя'::text AS note
UNION
 SELECT 'row_color'::text AS type,
    'Цвет строки'::text AS note
UNION
 SELECT 'filedb'::text AS type,
    'Файл в базе'::text AS note
UNION
 SELECT 'progress'::text AS type,
    'Горизонтальный индикатор'::text AS note
UNION
 SELECT 'invisible'::text AS type,
    'Скрытый'::text AS note;



CREATE VIEW meta.schema AS
 SELECT schemata.schema_name
   FROM information_schema.schemata
  WHERE ((schemata.schema_name)::text <> ALL (ARRAY[('pg_toast'::character varying)::text, ('pg_temp_1'::character varying)::text, ('pg_toast_temp_1'::character varying)::text, ('pg_catalog'::character varying)::text, ('information_schema'::character varying)::text]));




CREATE VIEW meta.user_list AS
 SELECT pg_authid.rolname
   FROM pg_authid;


CREATE VIEW meta.view_entity AS
 SELECT entity.entity,
    entity.title,
    entity.primarykey,
    entity.base_entity,
    entity.table_type,
    entity.hint
   FROM meta.entity
  WHERE (meta.entity_to_schema(entity.entity) <> ALL (ARRAY['ems'::text, 'meta'::text]));



CREATE VIEW meta.view_page AS
 SELECT page.page_key,
    page.title
   FROM meta.page;



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
            COALESCE(to_char(t1."order", '000'::text), '000'::text) AS path,
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
            ((temp1_1.path || '->'::text) || COALESCE(to_char(t2."order", '000'::text), '000'::text)),
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


CREATE VIEW meta.view_projection_buttons AS
 SELECT projection_buttons.button,
    projection_buttons.projection_name,
    projection_buttons.title,
    projection_buttons.icon,
    projection_buttons.function,
    projection_buttons.schema,
    projection_buttons.use_in_list
   FROM meta.projection_buttons;

CREATE VIEW meta.view_projection_entity AS
 SELECT projection_entity.projection_name,
    meta.entity_to_table(projection_entity.entity::text) AS table_name,
    meta.entity_to_schema(projection_entity.entity::text) AS table_schema,
    projection_entity.title,
    projection_entity.jump,
    projection_entity.primarykey,
    projection_entity.additional,
    projection_entity.readonly,
    projection_entity.hint
   FROM meta.projection_entity;


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
   
   
   
CREATE FUNCTION meta.entity_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
   old_entity text;
BEGIN
  old_entity := quote_ident(old.entity);
  IF old.table_type = 'VIEW' THEN
    EXECUTE('DROP VIEW '||old.entity||';');
  ELSE
    EXECUTE('DROP TABLE '||old.entity||';');
  END IF;
  PERFORM  meta.clean();  
  RETURN new;
END;$$;


CREATE FUNCTION meta.entity_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  name_key TEXT;
  new_schema TEXT;
  new_table TEXT;
  base_entity text;
  new_entity text;
  new_primarykey text;
  new_title text;
  new_hint text;
  new_view_definition text;

  
BEGIN
  new_entity = quote_ident(lower(new.entity)); 
  new_title = quote_ident(new.title);
  new_hint = quote_ident(new.hint);

  IF new_entity SIMILAR TO '[a-z][a-z,_,0-9]{0,62}.[a-z][a-z,_,0-9]{0,62}' THEN
    new_table  = "meta"."entity_to_table"(new_entity);
    new_schema = "meta"."entity_to_schema"(new_entity);
  ELSE IF new_entity SIMILAR TO '[a-z][a-z,_,0-9]{0,62}' THEN
    new_table  = new_entity;
    new_schema = 'public';
    new_entity = 'public'||'.'||new_table;
    IF NOT EXISTS(SELECT schema_name FROM information_schema.schemata WHERE schema_name = new_schema) THEN
	RAISE EXCEPTION 'Схемы - % не существует.',new_schema;
	RETURN new;
    END IF;
    IF (new.view_definition is NOT null) THEN -- добавление представления
      new_view_definition = quote_ident(new.view_definition);
      EXECUTE ( 'CREATE OR REPLACE VIEW '||new_entity||' AS ' || new_view_definition );
      --INSERT INTO meta.entity_extra(entity, base_entity, title,  hint, primarykey) 
      --   SELECT new_entity, base_entity.base_entity, new_title, new_hint, base_entity.primarykey 
      --                           FROM meta.base_entity WHERE base_entity.entity = new_entity limit 1;
    ELSE
      new_primarykey = quote_ident(new.primarykey);
      IF new_primarykey IS NULL THEN
        name_key := 'key';
      ELSE
        name_key := (new.primarykey);  
      END IF;
      EXECUTE('
       CREATE TABLE '||new.entity||' ("'||name_key||'" uuid default uuid_generate_v4(), CONSTRAINT "'||
		meta.entity_to_table(new.entity)||'_pkey"  PRIMARY KEY ("'||name_key||'"));'
	 );
     INSERT INTO meta.entity_extra(entity, base_entity, primarykey, title,  hint) SELECT new.entity, new.entity, name_key, new.title, new.hint;

    END IF;

  ELSE
     RAISE EXCEPTION 'Некорректное имя сущности - %',new.entity;
  END IF; 
  END IF;
         
 RETURN new;

 END;$$;




CREATE FUNCTION meta.entity_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS 
$$DECLARE
  new_title text;
  new_hint text;
  new_entity text;
  new_primarykey text;
  new_base_entity text;
  new_view_definition text;
BEGIN
  new_entity = quote_ident(new.entity);
  UPDATE meta.entity_extra set 
    title = new.title,
    primarykey = new.primarykey, 
    hint = new.hint
  WHERE entity_extra.entity = new.entity;

  IF (old.table_type = 'BASE TABLE') THEN

    new_title := quote_ident(new.title);
    new_hint := quote_ident(new.hint);
 
	  INSERT INTO meta.entity_extra(
        entity, 
        title, 
        base_entity, 
        primarykey, 
        hint) 
      SELECT 
        new.entity, 
        new.title, 
        new.entity,
	      new.primarykey, 
        new.hint
	    WHERE NOT exists (SELECT * FROM  meta.entity_extra WHERE entity_extra.entity = new.entity);
  ELSE
    new_view_definition := quote_ident(new.view_definition);
	  IF new.view_definition <> old.view_definition THEN
	    EXECUTE ( 'CREATE OR REPLACE VIEW '||new.entity||' AS ' || new.view_definition );
	  END IF;

	  INSERT INTO meta.entity_extra(
        entity, 
        title, 
        base_entity, 
        primarykey, 
        hint) 
      SELECT 
        new.entity, 
        new.title, 
        new.base_entity,
	      new.primarykey,
        new.hint
	    WHERE NOT exists (SELECT * FROM  meta.entity_extra WHERE entity_extra.entity = new.entity);

   END IF;
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
  new_function_schema text;
  new_function_name text;
  new_function_code text;

BEGIN
  new_function_schema := quote_ident(new.function_schema);
  new_function_name := quote_ident(new.function_name);
  new_function_code := quote_ident(new.function_code);

  EXECUTE '
CREATE OR REPLACE FUNCTION ' || new.function_schema || '.' || new.function_name || ' (row_key uuid, user_key uuid) RETURNS text AS $BODY$' || new.function_code || '$BODY$ LANGUAGE plpgsql VOLATILE NOT LEAKPROOF';
  RETURN new;
END;$_$;



CREATE FUNCTION meta.function_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$DECLARE
  new_function_schema text;
  new_function_name text;
  new_function_code text;

BEGIN
  new_function_schema := quote_ident(new.function_schema);
  new_function_name := quote_ident(new.function_name);
  new_function_code := quote_ident(new.function_code);

  PERFORM udf_dropfunction(old.function_name);
  EXECUTE '
CREATE OR REPLACE FUNCTION ' || new.function_schema || '.' || new.function_name || ' (row_key uuid, user_key uuid) RETURNS text AS $BODY$' || new.function_code || '$BODY$ LANGUAGE plpgsql VOLATILE NOT LEAKPROOF';
  RETURN new;
END;$_$;




CREATE FUNCTION meta.grants_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_entity text;
  new_groname text;

BEGIN
  new_entity := quote_ident(new.entity);
  new_groname := quote_ident(new.groname);

   IF new.INSERT <> old.INSERT THEN

     IF new.INSERT = true THEN
       execute ('GRANT INSERT ON TABLE '||new.entity||' TO '||new.groname);
     ELSE
       execute ('REVOKE INSERT ON TABLE '||new.entity||' FROM '||new.groname);
     END IF;   
   END IF;


   IF new.update <> old.update THEN
     IF new.update = true THEN
       execute ('GRANT UPDATE ON TABLE '||new.entity||' TO '||new.groname);
     ELSE
       execute ('REVOKE UPDATE ON TABLE '||new.entity||' FROM '||new.groname);
     END IF;   
   END IF;


   IF new.DELETE <> old.DELETE THEN
     IF new.DELETE = true THEN
       execute ('GRANT DELETE ON TABLE '||new.entity||' TO '||new.groname);
     ELSE
       execute ('REVOKE DELETE ON TABLE '||new.entity||' FROM '||new.groname);
     END IF;   
   END IF;


   IF new.SELECT <> old.SELECT THEN
     IF new.SELECT = true THEN
       execute ('GRANT SELECT ON TABLE '||new.entity||' TO '||new.groname);
     ELSE
       execute ('REVOKE SELECT ON TABLE '||new.entity||' FROM '||new.groname);
     END IF;   
   END IF;
   
   RETURN new;
END;$$;



CREATE FUNCTION meta.menu_item_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
 
 IF new.name is null THEN
  new.name = new.projection;
 END IF;
 
 IF new.title is null THEN
   new.title  = coalesce((SELECT projection_entity.title FROM meta.projection_entity WHERE projection_entity.projection_name = new.projection), new.projection);
 END IF;

 RETURN new;
END;$$;


CREATE FUNCTION meta.projection_entity_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
  DELETE FROM meta.projection_entity_extra WHERE projection_name = old.projection_name;
  RETURN old;
END;$$;



CREATE FUNCTION meta.projection_entity_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
 INSERT INTO meta.projection_entity_extra(projection_name, entity, title, additional, readonly, jump) SELECT new.projection_name, new.entity, new.title, new.additional, new.readonly, new.jump;
 RETURN new;
END;$$;


CREATE FUNCTION meta.projection_entity_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
 UPDATE meta.projection_entity_extra set title = new.title, jump = new.jump, additional = new.additional, readonly = new.readonly, hint = new.hint WHERE projection_entity_extra.projection_name = new.projection_name;
 UPDATE meta.entity set title = new.title, entity = new.entity, primarykey = new.primarykey WHERE meta.entity_to_table(entity.entity) = new.projection_name;
 INSERT INTO meta.projection_entity_extra(projection_name, entity, title , jump, additional, readonly, hint) SELECT new.projection_name, new.entity, new.title, new.jump, new.additional, new.readonly, new.hint WHERE NOT exists
   (SELECT * FROM  meta.projection_entity_extra WHERE projection_entity_extra.projection_name = new.projection_name);
 RETURN new;
END;$$;



CREATE FUNCTION meta.projection_property_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp text;
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
-- WHERE column_name = new.column_name and entity IN (SELECT substring(entity, '\w*')||'.'||projection_name FROM meta.projection_entity WHERE projection_name = old.projection_name);

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



CREATE FUNCTION meta.projection_redirect() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN
  new.projection_redirect := new.projection_name||'.'||new.role;
  RETURN new;
END;$$;


CREATE FUNCTION meta.projection_relation_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp_title text;
BEGIN
 UPDATE meta.projection_relation_extra set 
    title = new.title, 
    visible = new.visible, 
    readonly = new.readonly, 
    related_projection_name = new.related_projection_name,
    opened = new.opened,
    view_id = new.view_id,
    "order" = new.order,
    hint = new.hint
  WHERE projection_relation_extra.projection_relation_name = new.projection_relation_name RETURNING title INTO _temp_title;
  
 --UPDATE meta.relation set
 --   title = new.title,
 --   entity = new.entity,
 --   "order" = new."order"
 -- WHERE relation.relation_entity = new.relation_entity and relation.entity = new.projection_entity;
IF _temp_title IS NULL THEN 
 INSERT INTO meta.projection_relation_extra(
   projection_relation_name,
   projection_name, 
   relation_entity, 
   title, 
   visible, 
   readonly, 
   related_projection_name, 
   "order", 
   view_id,
   hint) 
    SELECT 
       new.projection_relation_name,
      new.projection_name, 
      new.relation_entity, 
      new.title, 
      new.visible, 
      new.readonly, 
      new.related_projection_name, 
      new.order, 
      new.view_id,
      new.hint 
    WHERE NOT exists
   (SELECT * FROM  meta.projection_relation_extra 
      WHERE projection_relation_extra.projection_relation_name = new.projection_name||'.'||new.entity||'.'||new.relation_entity||'.'||new.key);
END IF;
 RETURN new;   
END;$$;



CREATE FUNCTION meta.property_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  old_entity text;
  old_column_name text;

BEGIN
  old_entity := quote_ident(old.entity);
  old_column_name := quote_ident(old.column_name);

    EXECUTE('ALTER TABLE '||old_entity||' drop column '||old_column_name);
    EXECUTE('DELETE FROM meta.property_extra WHERE entity = '''||old_entity||'''');
    
  RETURN old;   
END;$$;



CREATE FUNCTION meta.property_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_entity text;
  new_column_name text;
  new_data_type text;
  new_ref_entity text;
  new_ref_key text;
BEGIN
  new_entity := quote_ident(new.entity);
  new_ref_entity := quote_ident(new.ref_entity);
  
   IF new.ref_entity is NOT null THEN
    SELECT primarykey FROM meta.entity WHERE entity = new.ref_entity INTO new.ref_key;
    
    new_ref_key := quote_ident(new.ref_key);
    IF(new.column_name is null)THEN
        new.column_name := new.ref_key;
    END IF;
    SELECT data_type FROM meta.property WHERE entity = new.ref_entity and column_name = new.ref_key INTO new.data_type;

    new_data_type := quote_ident(new.data_type);
    new_column_name := quote_ident(new.column_name);

    new.type = 'ref';
    EXECUTE('ALTER TABLE '||new_entity||' add column "'||new_column_name||'" '||new_data_type||';');
    EXECUTE('ALTER TABLE '||new_entity||' 
      ADD CONSTRAINT '||meta.entity_to_table(new_entity)||'_'||new_column_name||'_fkey FOREIGN KEY ("'||new_column_name||'")
          REFERENCES '||new_ref_entity||' ('||new_ref_key||') MATCH SIMPLE
          ON UPDATE NO ACTION ON DELETE NO ACTION;');
    IF NOT EXISTS(SELECT * FROM meta.property_extra WHERE column_name  = new.column_name and entity = new.entity) 
      THEN
     INSERT INTO meta.property_extra(entity, column_name, title, visible, readonly, type, ref_key, link_key, ref_entity, "order", hint, pattern) 
        SELECT new.entity, new.column_name, new.title, new.visible, new.readonly, new.type, new.ref_key, new.link_key, new.ref_entity, new."order", new.hint, new.pattern;
    END IF;
        
  ELSE
    IF new.data_type IS NULL THEN
      new.data_type = 'text';
    END IF;
      EXECUTE 'ALTER TABLE '||new.entity||' add column '||new.column_name||'  '||new.data_type||'';
      INSERT INTO meta.property_extra(entity, column_name, title, visible, readonly, type, "order", hint, pattern) 
        SELECT new.entity, new.column_name, new.title, new.visible, new.readonly, new.type, new."order", new.hint, new.pattern;
  END IF;
  RETURN new;      
END;$$;



CREATE FUNCTION meta.property_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  old_entity text;
  old_constraint_name text;
  old_column_name text;
  new_entity text;
  new_column_name text;
  new_ref_entity text;
  new_ref_key text;
  new_data_type text;

BEGIN
  old_entity := quote_ident(meta.entity_to_schema(old.entity)) || '.' || quote_ident(meta.entity_to_table(old.entity));
  old_constraint_name := quote_ident(old.constraint_name);
  new_entity := quote_ident(meta.entity_to_schema(new.entity)) || '.' || quote_ident(meta.entity_to_table(new.entity));

IF exists(SELECT * FROM meta.entity WHERE entity = new.entity and table_type = 'r')THEN
   IF old.ref_entity is NOT null and (old.ref_entity<>new.ref_entity or new.ref_entity is null) THEN

--RAISE notice 'q1 - %', 'ALTER TABLE '||old.entity||'     DROP CONSTRAINT "'||old.constraint_name||'"';
  IF (old.constraint_name is NOT NULL) THEN
    EXECUTE('ALTER TABLE '||old.entity||'
      DROP CONSTRAINT  "'||old.constraint_name||'"');
        new.ref_key := null;
    END IF;
  END IF;
  IF new.ref_entity is NOT null and (old.ref_entity<>new.ref_entity or old.ref_entity is null) THEN
  SELECT primarykey FROM meta.entity WHERE entity = new.ref_entity INTO new.ref_key;

  --new_column_name := quote_ident(new.column_name);
  new_ref_entity := quote_ident(meta.entity_to_schema(new.ref_entity)) || '.' || quote_ident(meta.entity_to_table(new.ref_entity::text));
  new_entity := meta.entity_to_table(new.entity);
  
  EXECUTE('ALTER TABLE '||old_entity||'
    ADD CONSTRAINT "'||new_entity||'_'||new.column_name||'_fkey" FOREIGN KEY ("'||new.column_name||'")
        REFERENCES '||new_ref_entity||' ("'||new.ref_key||'") MATCH SIMPLE
        ON UPDATE NO ACTION ON DELETE NO ACTION;');
  END IF;
END IF;
IF new.data_type <> old.data_type THEN
--RAISE EXCEPTION 'q1 - %', old.entity;

  old_column_name := quote_ident(old.column_name);
  new_data_type := quote_ident(new.data_type);

  EXECUTE 'alter table '||old_entity||' alter column '||old_column_name||' type '||new_data_type||' using ('||old_column_name||'::'||new_data_type||')       ';

END IF;
 UPDATE meta.property_extra set 
     title = new.title, 
     visible = new.visible, 
     readonly = new.readonly, 
     type = new.type, 
     ref_key = new.ref_key, 
     link_key = new.link_key, 
     ref_entity = new.ref_entity,
     "order" = new.order,
     hint = new.hint,
     pattern = new.pattern
   WHERE property_extra.entity = new.entity and property_extra.column_name = new.column_name ;
 INSERT INTO meta.property_extra(entity,  column_name, title, visible, readonly, type, ref_key, link_key, ref_entity, "order", hint, pattern) 
    SELECT 
     old.entity,
     old.column_name,
           new.title, 
           new.visible, 
           new.readonly, 
     new.type, 
     new.ref_key, 
     new.link_key,
     new.ref_entity,
     new.order,
           new.hint,
           new.pattern
    WHERE NOT exists
(SELECT * FROM  meta.property_extra WHERE property_extra.entity = old.entity and property_extra.column_name = old.column_name);

 RETURN new;   
END;$$;



CREATE FUNCTION meta.relation_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp text;
BEGIN
 UPDATE meta.relation_extra set 
    title = new.title, 
    "order" = new.order,
    hint = new.hint
WHERE relation_extra.relation_name = new.relation_name RETURNING relation_name INTO _temp;

IF _temp IS NULL THEN
 INSERT INTO meta.relation_extra(relation_name, title,  "order", hint) 
   SELECT
      new.entity||'.'||new.relation_entity||'.'||new.key as relation_name,
      new.title,
      new.order,
      new.hint
    WHERE NOT exists
   (SELECT * FROM  meta.relation_extra WHERE relation_extra.relation_name = new.relation_name);
END IF;
 RETURN new;   

END;$$;





ALTER TABLE ONLY meta.entity_extra
    ADD CONSTRAINT entity_extra_pkey PRIMARY KEY (entity);

ALTER TABLE ONLY meta.menu_item
    ADD CONSTRAINT menu_item_pkey PRIMARY KEY (name);

ALTER TABLE ONLY meta.page_block
    ADD CONSTRAINT page_block_pkey PRIMARY KEY (block_key);

ALTER TABLE ONLY meta.page
    ADD CONSTRAINT page_pkey PRIMARY KEY (page_key);

ALTER TABLE ONLY meta.pivot
    ADD CONSTRAINT pivot_pkey PRIMARY KEY (entity);

ALTER TABLE ONLY meta.projection_buttons
    ADD CONSTRAINT projection_buttons_pkey PRIMARY KEY (button, projection_name);

ALTER TABLE ONLY meta.projection_entity_extra
    ADD CONSTRAINT projection_entity_extra_pkey PRIMARY KEY (projection_name, entity);

ALTER TABLE ONLY meta.projection_property_extra
    ADD CONSTRAINT projection_property_extra_pkey PRIMARY KEY (projection_name, column_name);

ALTER TABLE ONLY meta.projection_redirect
    ADD CONSTRAINT projection_redirect_pkey PRIMARY KEY (projection_redirect);

ALTER TABLE ONLY meta.projection_relation_extra
    ADD CONSTRAINT projection_relation_extra_pkey PRIMARY KEY (projection_relation_name);

ALTER TABLE ONLY meta.property_extra
    ADD CONSTRAINT property_extra_pkey PRIMARY KEY (column_name, entity);

ALTER TABLE ONLY meta.relation_add
    ADD CONSTRAINT relation_add_pkey PRIMARY KEY (relation_name);

ALTER TABLE ONLY meta.relation_extra
    ADD CONSTRAINT relation_extra_pkey PRIMARY KEY (relation_name);


CREATE INDEX fki_menu_item_fk ON meta.menu_item USING btree (parent);

CREATE TRIGGER entity_DELETE_trg INSTEAD OF DELETE ON meta.entity FOR EACH ROW EXECUTE PROCEDURE meta.entity_DELETE_trgf();

CREATE TRIGGER entity_insert_trg INSTEAD OF INSERT ON meta.entity FOR EACH ROW EXECUTE PROCEDURE meta.entity_insert_trgf();

CREATE TRIGGER entity_update_trg INSTEAD OF UPDATE ON meta.entity FOR EACH ROW EXECUTE PROCEDURE meta.entity_update_trgf();

CREATE TRIGGER function_DELETE_trg INSTEAD OF DELETE ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_DELETE_trgf();

CREATE TRIGGER function_insert_trg INSTEAD OF INSERT ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_insert_trgf();

CREATE TRIGGER function_update_trg INSTEAD OF UPDATE ON meta.functions FOR EACH ROW EXECUTE PROCEDURE meta.function_update_trgf();

--CREATE TRIGGER grants_update_trg INSTEAD OF UPDATE ON meta.grants FOR EACH ROW EXECUTE PROCEDURE meta.grants_update_trgf();

CREATE TRIGGER menu_item_tr BEFORE INSERT ON meta.menu_item FOR EACH ROW EXECUTE PROCEDURE meta.menu_item_insert_trgf();

CREATE TRIGGER projection_entity_DELETE_trg INSTEAD OF DELETE ON meta.projection_entity FOR EACH ROW EXECUTE PROCEDURE meta.projection_entity_DELETE_trgf();

CREATE TRIGGER projection_entity_insert_trg INSTEAD OF INSERT ON meta.projection_entity FOR EACH ROW EXECUTE PROCEDURE meta.projection_entity_insert_trgf();

CREATE TRIGGER projection_entity_update_trg INSTEAD OF UPDATE ON meta.projection_entity FOR EACH ROW EXECUTE PROCEDURE meta.projection_entity_update_trgf();

CREATE TRIGGER projection_property_update_trg INSTEAD OF INSERT OR UPDATE ON meta.projection_property FOR EACH ROW EXECUTE PROCEDURE meta.projection_property_update_trgf();

CREATE TRIGGER projection_redirect_trg BEFORE INSERT OR UPDATE ON meta.projection_redirect FOR EACH ROW EXECUTE PROCEDURE meta.projection_redirect();

CREATE TRIGGER projection_relation_update_trg INSTEAD OF INSERT OR UPDATE ON meta.projection_relation FOR EACH ROW EXECUTE PROCEDURE meta.projection_relation_update_trgf();

CREATE TRIGGER property_DELETE_trg INSTEAD OF DELETE ON meta.property FOR EACH ROW EXECUTE PROCEDURE meta.property_DELETE_trgf();

CREATE TRIGGER property_insert_trg INSTEAD OF INSERT ON meta.property FOR EACH ROW EXECUTE PROCEDURE meta.property_insert_trgf();

CREATE TRIGGER property_update_trg INSTEAD OF UPDATE ON meta.property FOR EACH ROW EXECUTE PROCEDURE meta.property_update_trgf();

CREATE TRIGGER relation_update_trg INSTEAD OF INSERT OR UPDATE ON meta.relation FOR EACH ROW EXECUTE PROCEDURE meta.relation_update_trgf();

   
   

