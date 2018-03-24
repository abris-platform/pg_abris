-- complain IF script is sourced IN psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pgabris" to load this file. \quit

CREATE SCHEMA meta;

CREATE TABLE meta.entity_extra (
    title text NOT NULL,
    primarykey text,
    ems_en integer DEFAULT 0,
    entity text NOT NULL,
    base_entity text NOT NULL,
    hint text
);

CREATE VIEW meta.entity AS
 SELECT (((n.nspname)::text || '.'::text) || (v.relname)::text) AS entity,
    COALESCE(ee.title, (v.relname)::text) AS title,
    COALESCE(ee.primarykey, b.base_entity_key) AS primarykey,
    COALESCE(ee.base_entity, b.base_entity) AS base_entity,
    (v.relkind)::information_schema.character_data AS table_type,
    COALESCE(ee.ems_en, 0) AS ems_en,
    ee.hint,
    (pg_get_viewdef(v.oid))::information_schema.character_data AS view_definition,
    v.oid AS entity_id,
    b.base_entity_key,
    b.base_entity_id
   FROM (((pg_class v
     LEFT JOIN pg_namespace n ON ((n.oid = v.relnamespace)))
     LEFT JOIN ( SELECT t.refobjid AS entity_id,
            t.obj AS base_entity_id,
            (((ns.nspname)::text || '.'::text) || (n_1.relname)::text) AS base_entity,
            (at.attname)::text AS base_entity_key
           FROM ((((( SELECT DISTINCT dv.refobjid,
                    min(dt.refobjid) AS obj
                   FROM (pg_depend dv
                     JOIN pg_depend dt ON (((dv.objid = dt.objid) AND (dv.refobjid <> dt.refobjid) AND (dt.classid = ('pg_rewrite'::regclass)::oid) AND (dt.refclassid = ('pg_class'::regclass)::oid))))
                  WHERE ((dv.refclassid = ('pg_class'::regclass)::oid) AND (dv.classid = ('pg_rewrite'::regclass)::oid) AND (dv.deptype = 'i'::"char"))
                  GROUP BY dv.refobjid) t
             JOIN pg_class n_1 ON (((n_1.oid = t.obj) AND (n_1.relkind = 'r'::"char"))))
             LEFT JOIN pg_constraint c ON (((c.conrelid = n_1.oid) AND (c.contype = 'p'::"char"))))
             LEFT JOIN pg_attribute at ON (((c.conkey[1] = at.attnum) AND (at.attrelid = c.conrelid))))
             LEFT JOIN pg_namespace ns ON ((ns.oid = n_1.relnamespace)))) b ON ((v.oid = b.entity_id)))
     LEFT JOIN entity_extra ee ON ((ee.entity = (((n.nspname)::text || '.'::text) || (v.relname)::text))))
  WHERE ((v.relkind = 'v'::"char") AND pg_has_role(v.oid, 'USAGE'::text) AND (n.nspname <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])))
UNION ALL
 SELECT (((n.nspname)::text || '.'::text) || (r.relname)::text) AS entity,
    COALESCE(ee.title, (r.relname)::text) AS title,
    at.attname AS primarykey,
    (((n.nspname)::text || '.'::text) || (r.relname)::text) AS base_entity,
    (r.relkind)::information_schema.character_data AS table_type,
    COALESCE(ee.ems_en, 0) AS ems_en,
    ee.hint,
    (NULL::character varying)::information_schema.character_data AS view_definition,
    r.oid AS entity_id,
    NULL::name AS base_entity_key,
    NULL::oid AS base_entity_id
   FROM ((((pg_class r
     LEFT JOIN pg_namespace n ON ((n.oid = r.relnamespace)))
     LEFT JOIN pg_constraint c ON (((c.conrelid = r.oid) AND (c.contype = 'p'::"char"))))
     LEFT JOIN pg_attribute at ON (((c.conkey[1] = at.attnum) AND (at.attrelid = c.conrelid))))
     LEFT JOIN entity_extra ee ON ((ee.entity = (((n.nspname)::text || '.'::text) || (r.relname)::text))))
  WHERE ((r.relkind = 'r'::"char") AND pg_has_role(r.oid, 'USAGE'::text) AND (n.nspname <> ALL (ARRAY['pg_catalog'::name, 'information_schema'::name])));


CREATE FUNCTION meta.clean() RETURNS text
    LANGUAGE plpgsql
    AS $$begin
  DELETE FROM meta.entity_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.property_extra WHERE entity||'.'||column_name NOT IN (SELECT entity||'.'||column_name FROM meta.property);
  DELETE FROM meta.relation_extra WHERE relation_name NOT IN (SELECT relation_name FROM meta.relation);
  DELETE FROM meta.projection_entity_extra WHERE entity NOT IN (SELECT entity FROM meta.entity);
  DELETE FROM meta.projection_property_extra WHERE projection_property_name NOT IN (SELECT projection_property_name FROM meta.projection_property);
  DELETE FROM meta.projection_relation_extra WHERE projection_relation_name NOT IN (SELECT projection_relation_name FROM meta.projection_relation);
  DELETE FROM meta.menu_item WHERE projection NOT IN (SELECT projection_name FROM meta.projection_entity);
  return 'Метаданные успешно очищены';
end;$$;

CREATE FUNCTION entity_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
old_entity text;

begin
  old_entity := quote_ident(old.entity);
  if old.table_type = 'VIEW' then
    EXECUTE('DROP VIEW '||old.entity||';');
  else
    EXECUTE('DROP TABLE '||old.entity||';');
  end if;
  PERFORM  meta.clean();  
  return new;
end;$$;


ALTER FUNCTION meta.entity_DELETE_trgf() OWNER TO postgres;

--
-- TOC entry 617 (class 1255 OID 405459)
-- Name: entity_insert_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION entity_insert_trgf() RETURNS trigger
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

      insert into meta.entity_extra(entity, base_entity, title,  hint, primarykey) SELECT new_entity, base_entity.base_entity, new_title, new_hint, base_entity.primarykey 
                                 FROM meta.base_entity WHERE base_entity.entity = new_entity limit 1;


    
    else

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


        insert into meta.entity_extra(entity, base_entity, primarykey, title,  hint) SELECT new.entity, new.entity, name_key, new.title, new.hint;

    end if;

  ELSE
     RAISE EXCEPTION 'Некорректное имя сущности - %',new.entity;
  END IF; 
  end if;
         
 return new;

 end;$$;


ALTER FUNCTION meta.entity_insert_trgf() OWNER TO postgres;

--
-- TOC entry 588 (class 1255 OID 405460)
-- Name: entity_to_schema(text); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION entity_to_schema(parameter text) RETURNS text
    LANGUAGE plpgsql
    AS $$
        DECLARE
          result text;
        
        BEGIN
          result := substring(parameter, '\w*'::text);
          RETURN result;
        END;
    $$;


ALTER FUNCTION meta.entity_to_schema(parameter text) OWNER TO postgres;

--
-- TOC entry 593 (class 1255 OID 405461)
-- Name: entity_to_table(text); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION entity_to_table(parameter text) RETURNS text
    LANGUAGE plpgsql
    AS $$
        DECLARE
          result text;
        
        BEGIN
          result := substring(parameter, '\.(\w*)'::text);
          RETURN result;
        END;
    $$;


ALTER FUNCTION meta.entity_to_table(parameter text) OWNER TO postgres;

--
-- TOC entry 618 (class 1255 OID 405462)
-- Name: entity_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION entity_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_title text;
  new_ems_en text;
  new_hint text;
  new_entity text;
  new_primarykey text;
  new_base_entity text;
  new_view_definition text;

begin
  
  new_entity := quote_ident(new.entity);

   IF (old.table_type = 'BASE TABLE') THEN

    new_title := quote_ident(new.title);
    new_ems_en := quote_ident(new.ems_en);
    new_hint := quote_ident(new.hint);

	   update meta.entity_extra set 
	      title = new.title, 
	      ems_en = new.ems_en,
	      hint = new.hint
	   WHERE entity_extra.entity = new.entity;
   
	   insert into meta.entity_extra(entity, title, base_entity, primarykey, ems_en, hint) SELECT new.entity, new.title, new.entity,
	    coalesce(new.primarykey, (SELECT distinct primarykey FROM meta.base_entity WHERE entity = new.entity and base_entity = new.base_entity)), 
	    new.ems_en, new.hint
	     WHERE NOT exists (SELECT * FROM  meta.entity_extra WHERE entity_extra.entity = new.entity);
    ELSE
    new_view_definition := quote_ident(new.view_definition);
	   if new.view_definition <> old.view_definition then
	       EXECUTE ( 'CREATE OR REPLACE VIEW '||new.entity||' AS ' || new.view_definition );


		     IF NOT EXISTS(SELECT base_entity.base_entity 
						 FROM meta.base_entity WHERE base_entity.entity = new.entity AND base_entity.base_entity = new.base_entity) then
						 
			      SELECT base_entity.base_entity 
						 FROM meta.base_entity WHERE base_entity.entity = new.entity into new.base_entity limit 1;
		     end if;
	   end if;

   
	   update meta.entity_extra set 
	      title = new.title, 
	      base_entity = new.base_entity, 
	      primarykey = (SELECT distinct primarykey FROM meta.base_entity WHERE entity = new.entity and base_entity = new.base_entity),
	      ems_en = new.ems_en,
	      hint = new.hint
	   WHERE entity_extra.entity = new.entity;
   
	   insert into meta.entity_extra(entity, title, base_entity, primarykey, hint) SELECT new.entity, new.title, new.base_entity,
	    coalesce(new.primarykey, (SELECT distinct primarykey FROM meta.base_entity WHERE entity = new.entity and base_entity = new.base_entity)), new.hint
	     WHERE NOT exists (SELECT * FROM  meta.entity_extra WHERE entity_extra.entity = new.entity);


   END IF;
   if new.ems_en <> old.ems_en then

     perform ems.history_enable_for_table(new::meta.entity);
   end if;


 return new;
end;$$;


ALTER FUNCTION meta.entity_update_trgf() OWNER TO postgres;

--
-- TOC entry 619 (class 1255 OID 405463)
-- Name: function_DELETE_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION function_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$BEGIN

  PERFORM udf_dropfunction(old.function_name);
  RETURN old;
END;$$;


ALTER FUNCTION meta.function_DELETE_trgf() OWNER TO postgres;

--
-- TOC entry 620 (class 1255 OID 405464)
-- Name: function_insert_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION function_insert_trgf() RETURNS trigger
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


ALTER FUNCTION meta.function_insert_trgf() OWNER TO postgres;

--
-- TOC entry 621 (class 1255 OID 405465)
-- Name: function_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION function_update_trgf() RETURNS trigger
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


ALTER FUNCTION meta.function_update_trgf() OWNER TO postgres;

--
-- TOC entry 736 (class 1255 OID 462279)
-- Name: get_hint_column(text, text); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION get_hint_column(entity text, "column" text) RETURNS text
    LANGUAGE plpgsql
    AS $$DECLARE
    r  text;
    e   text;
    t text;
    n text;

begin
execute ('SELECT hint_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into t;
execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into n;
execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into e;

execute ('SELECT '||t||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') into r;
return r; 
end;$$;


ALTER FUNCTION meta.get_hint_column(entity text, "column" text) OWNER TO postgres;

--
-- TOC entry 734 (class 1255 OID 462278)
-- Name: get_name_column(text, text); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION get_name_column(entity text, "column" text) RETURNS text
    LANGUAGE plpgsql
    AS $$DECLARE
    r  text;
    e   text;
    t text;
    n text;

begin
execute ('SELECT title_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into t;
execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into n;
execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into e;

execute ('SELECT '||t||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') into r;
return r; 

end;$$;


ALTER FUNCTION meta.get_name_column(entity text, "column" text) OWNER TO postgres;

--
-- TOC entry 739 (class 1255 OID 462282)
-- Name: get_order_column(text, text); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION get_order_column(entity text, "column" text) RETURNS integer
    LANGUAGE plpgsql
    AS $$DECLARE
    r  integer;
    e   text;
    t text;
    n text;

begin
execute ('SELECT hint_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into t;
execute ('SELECT num_column FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into n;
execute ('SELECT entity_row FROM meta.pivot WHERE entity = '''||entity||''' limit 1') into e;

execute ('SELECT '||n||' FROM '||e||' WHERE ''c''||'||n||' = '''||"column"||'''  limit 1') into r;
return 100+r; 
end;$$;


ALTER FUNCTION meta.get_order_column(entity text, "column" text) OWNER TO postgres;

--
-- TOC entry 622 (class 1255 OID 405466)
-- Name: grants_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION grants_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_entity text;
  new_groname text;

begin
  new_entity := quote_ident(new.entity);
  new_groname := quote_ident(new.groname);

   if new.insert <> old.insert then

     if new.insert = true then
       execute ('GRANT INSERT ON TABLE '||new.entity||' TO '||new.groname);
     else
       execute ('REVOKE INSERT ON TABLE '||new.entity||' FROM '||new.groname);
     end if;   
   end if;


   if new.update <> old.update then
     if new.update = true then
       execute ('GRANT UPDATE ON TABLE '||new.entity||' TO '||new.groname);
     else
       execute ('REVOKE UPDATE ON TABLE '||new.entity||' FROM '||new.groname);
     end if;   
   end if;


   if new.DELETE <> old.DELETE then
     if new.DELETE = true then
       execute ('GRANT DELETE ON TABLE '||new.entity||' TO '||new.groname);
     else
       execute ('REVOKE DELETE ON TABLE '||new.entity||' FROM '||new.groname);
     end if;   
   end if;


   if new.SELECT <> old.SELECT then
     if new.SELECT = true then
       execute ('GRANT SELECT ON TABLE '||new.entity||' TO '||new.groname);
     else
       execute ('REVOKE SELECT ON TABLE '||new.entity||' FROM '||new.groname);
     end if;   
   end if;
   
   return new;
end;$$;


ALTER FUNCTION meta.grants_update_trgf() OWNER TO postgres;

--
-- TOC entry 623 (class 1255 OID 405467)
-- Name: menu_item_insert_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION menu_item_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
 if new.name is null then
  new.name = new.projection;
 end if;
new.title  = (SELECT projection_entity.title FROM meta.projection_entity WHERE projection_entity.projection_name = new.projection);
 return new;
end;$$;


ALTER FUNCTION meta.menu_item_insert_trgf() OWNER TO postgres;

--
-- TOC entry 624 (class 1255 OID 405468)
-- Name: projection_entity_DELETE_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_entity_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
  DELETE FROM meta.projection_entity_extra WHERE projection_name = old.projection_name;
  return old;
end;$$;


ALTER FUNCTION meta.projection_entity_DELETE_trgf() OWNER TO postgres;

--
-- TOC entry 625 (class 1255 OID 405469)
-- Name: projection_entity_insert_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_entity_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
 insert into meta.projection_entity_extra(projection_name, entity, title, additional, readonly, jump) SELECT new.projection_name, new.entity, new.title, new.additional, new.readonly, new.jump;
 return new;
end;$$;


ALTER FUNCTION meta.projection_entity_insert_trgf() OWNER TO postgres;

--
-- TOC entry 626 (class 1255 OID 405470)
-- Name: projection_entity_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_entity_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
 update meta.projection_entity_extra set title = new.title, jump = new.jump, additional = new.additional, readonly = new.readonly, hint = new.hint WHERE projection_entity_extra.projection_name = new.projection_name;
 update meta.entity set title = new.title, entity = new.entity, primarykey = new.primarykey WHERE meta.entity_to_table(entity.entity) = new.projection_name;
 insert into meta.projection_entity_extra(projection_name, entity, title , jump, additional, readonly, hint) SELECT new.projection_name, new.entity, new.title, new.jump, new.additional, new.readonly, new.hint WHERE NOT exists
   (SELECT * FROM  meta.projection_entity_extra WHERE projection_entity_extra.projection_name = new.projection_name);
 return new;
end;$$;


ALTER FUNCTION meta.projection_entity_update_trgf() OWNER TO postgres;

--
-- TOC entry 627 (class 1255 OID 405471)
-- Name: projection_property_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_property_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp text;
begin
 update meta.projection_property_extra set 
        title = new.title, 
  visible = new.visible, 
  readonly = new.readonly, 
  type = new.type,
  ref_projection = new.ref_projection,
  "order" = new."order",
  concat_prev = new.concat_prev,
  hint = new.hint
 WHERE projection_property_extra.projection_property_name = new.projection_property_name RETURNING projection_property_name INTO _temp;

/*update meta.property set title = new.title,
  visible = new.visible,
  readonly = new.readonly,
  type = new.type,
  ref_key = new.ref_key,
  link_key = new.link_key,
  ref_entity = new.ref_entity,
  "order" = new."order",
  hint = new.hint
 WHERE column_name = new.column_name and entity IN (SELECT substring(entity, '\w*')||'.'||projection_name FROM meta.projection_entity WHERE projection_name = old.projection_name);
 */
 IF _temp IS NULL THEN
 insert into meta.projection_property_extra(
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
     new.hint/*
    WHERE NOT exists
   (SELECT * FROM  meta.projection_property_extra WHERE projection_property_extra.projection_property_name = new.projection_property_name)*/;
 END IF;
 return new;      
end;$$;


ALTER FUNCTION meta.projection_property_update_trgf() OWNER TO postgres;

--
-- TOC entry 628 (class 1255 OID 405472)
-- Name: projection_redirect(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_redirect() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
  new.projection_redirect := new.projection_name||'.'||new.role;
  return new;
end;$$;


ALTER FUNCTION meta.projection_redirect() OWNER TO postgres;

--
-- TOC entry 629 (class 1255 OID 405473)
-- Name: projection_relation_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION projection_relation_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp_title text;
begin
 update meta.projection_relation_extra set 
    title = new.title, 
    visible = new.visible, 
    readonly = new.readonly, 
    related_projection_name = new.related_projection_name,
    opened = new.opened,
    view_id = new.view_id,
    "order" = new.order,
    hint = new.hint
  WHERE projection_relation_extra.projection_relation_name = new.projection_relation_name RETURNING title INTO _temp_title;
  
 --update meta.relation set
 --   title = new.title,
 --   entity = new.entity,
 --   "order" = new."order"
 -- WHERE relation.relation_entity = new.relation_entity and relation.entity = new.projection_entity;
IF _temp_title IS NULL THEN 
 insert into meta.projection_relation_extra(
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
 return new;   
end;$$;


ALTER FUNCTION meta.projection_relation_update_trgf() OWNER TO postgres;

--
-- TOC entry 630 (class 1255 OID 405474)
-- Name: property_DELETE_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION property_DELETE_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  old_entity text;
  old_column_name text;

begin
  old_entity := quote_ident(old.entity);
  old_column_name := quote_ident(old.column_name);

    EXECUTE('ALTER TABLE '||old_entity||' drop column '||old_column_name);
    EXECUTE('DELETE FROM meta.property_extra WHERE entity = '''||old_entity||'''');
    
  return old;   
end;$$;


ALTER FUNCTION meta.property_DELETE_trgf() OWNER TO postgres;

--
-- TOC entry 631 (class 1255 OID 405475)
-- Name: property_insert_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION property_insert_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$DECLARE
  new_entity text;
  new_column_name text;
  new_data_type text;
  new_ref_entity text;
  new_ref_key text;
begin
  new_entity := quote_ident(new.entity);
  new_ref_entity := quote_ident(new.ref_entity);
  
   if new.ref_entity is NOT null then
    SELECT primarykey FROM meta.entity WHERE entity = new.ref_entity into new.ref_key;
    
    new_ref_key := quote_ident(new.ref_key);
    if(new.column_name is null)then
        new.column_name := new.ref_key;
    end if;
    SELECT data_type FROM meta.property WHERE entity = new.ref_entity and column_name = new.ref_key into new.data_type;

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
     insert into meta.property_extra(entity, column_name, title, visible, readonly, type, ref_key, link_key, ref_entity, "order", hint, pattern) 
        SELECT new.entity, new.column_name, new.title, new.visible, new.readonly, new.type, new.ref_key, new.link_key, new.ref_entity, new."order", new.hint, new.pattern;
    END IF;
        
  else
    IF new.data_type IS NULL THEN
      new.data_type = 'text';
    END IF;
      EXECUTE 'ALTER TABLE '||new.entity||' add column '||new.column_name||'  '||new.data_type||'';
      insert into meta.property_extra(entity, column_name, title, visible, readonly, type, "order", hint, pattern) 
        SELECT new.entity, new.column_name, new.title, new.visible, new.readonly, new.type, new."order", new.hint, new.pattern;
  end if;
  return new;      
end;$$;


ALTER FUNCTION meta.property_insert_trgf() OWNER TO postgres;

--
-- TOC entry 733 (class 1255 OID 405476)
-- Name: property_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION property_update_trgf() RETURNS trigger
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

begin
  old_entity := quote_ident(meta.entity_to_schema(old.entity)) || '.' || quote_ident(meta.entity_to_table(old.entity));
  old_constraint_name := quote_ident(old.constraint_name);
  new_entity := quote_ident(meta.entity_to_schema(new.entity)) || '.' || quote_ident(meta.entity_to_table(new.entity));

if exists(SELECT * FROM meta.entity WHERE entity = new.entity and table_type = 'r')then
   if old.ref_entity is NOT null and (old.ref_entity<>new.ref_entity or new.ref_entity is null) then

--RAISE notice 'q1 - %', 'ALTER TABLE '||old.entity||'     DROP CONSTRAINT "'||old.constraint_name||'"';
  IF (old.constraint_name is NOT NULL) THEN
    EXECUTE('ALTER TABLE '||old.entity||'
      DROP CONSTRAINT  "'||old.constraint_name||'"');
        new.ref_key := null;
    END IF;
  end if;
  if new.ref_entity is NOT null and (old.ref_entity<>new.ref_entity or old.ref_entity is null) then
  SELECT primarykey FROM meta.entity WHERE entity = new.ref_entity into new.ref_key;

  --new_column_name := quote_ident(new.column_name);
  new_ref_entity := quote_ident(meta.entity_to_schema(new.ref_entity)) || '.' || quote_ident(meta.entity_to_table(new.ref_entity));
  new_entity := meta.entity_to_table(new.entity);
  
  EXECUTE('ALTER TABLE '||old_entity||'
    ADD CONSTRAINT "'||new_entity||'_'||new.column_name||'_fkey" FOREIGN KEY ("'||new.column_name||'")
        REFERENCES '||new_ref_entity||' ("'||new.ref_key||'") MATCH SIMPLE
        ON UPDATE NO ACTION ON DELETE NO ACTION;');
  end if;
end if;
if new.data_type <> old.data_type then
--RAISE EXCEPTION 'q1 - %', old.entity;

  old_column_name := quote_ident(old.column_name);
  new_data_type := quote_ident(new.data_type);

  EXECUTE 'alter table '||old_entity||' alter column '||old_column_name||' type '||new_data_type||' using ('||old_column_name||'::'||new_data_type||')       ';

end if;
 update meta.property_extra set 
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
 insert into meta.property_extra(entity,  column_name, title, visible, readonly, type, ref_key, link_key, ref_entity, "order", hint, pattern) 
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

 return new;   
end;$$;


ALTER FUNCTION meta.property_update_trgf() OWNER TO postgres;

--
-- TOC entry 632 (class 1255 OID 405477)
-- Name: relation_update_trgf(); Type: FUNCTION; Schema: meta; Owner: postgres
--

CREATE FUNCTION relation_update_trgf() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
_temp text;
begin
 update meta.relation_extra set 
    title = new.title, 
    "order" = new.order,
    ems_en = new.ems_en,
    hint = new.hint
WHERE relation_extra.relation_name = new.relation_name RETURNING relation_name INTO _temp;

IF _temp IS NULL THEN
 insert into meta.relation_extra(relation_name, title, ems_en, "order", hint) 
   SELECT
      new.entity||'.'||new.relation_entity||'.'||new.key as relation_name,
      new.title,
      new.ems_en, 
      new.order,
      new.hint
    WHERE NOT exists
   (SELECT * FROM  meta.relation_extra WHERE relation_extra.relation_name = new.relation_name);
END IF;
 return new;   

end;$$;


ALTER FUNCTION meta.relation_update_trgf() OWNER TO postgres;

--
-- TOC entry 273 (class 1259 OID 405803)
-- Name: projection_entity_extra; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE projection_entity_extra (
    title text,
    projection_name text NOT NULL,
    jump text,
    additional text,
    readonly boolean DEFAULT false,
    entity text NOT NULL,
    hint text
);


ALTER TABLE projection_entity_extra OWNER TO postgres;

--
-- TOC entry 274 (class 1259 OID 405810)
-- Name: projection_entity; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW projection_entity AS
 SELECT COALESCE(projection_entity_extra.projection_name, entity_to_table(entity.entity)) AS projection_name,
    COALESCE(projection_entity_extra.entity, entity.entity) AS entity,
    COALESCE(projection_entity_extra.title, entity.title) AS title,
    COALESCE(projection_entity_extra.jump, COALESCE(projection_entity_extra.projection_name, entity_to_table(entity.base_entity))) AS jump,
    entity.primarykey,
    projection_entity_extra.additional,
    COALESCE(projection_entity_extra.readonly, (NOT has_table_privilege(entity.entity, 'insert, update, DELETE'::text))) AS readonly,
    (0)::bigint AS amount_dup,
    entity.base_entity,
    projection_entity_extra.hint,
    entity.ems_en
   FROM (entity
     LEFT JOIN projection_entity_extra ON ((projection_entity_extra.entity = entity.entity)))
  WHERE (EXISTS ( SELECT projection_entity_extra_1.projection_name
           FROM projection_entity_extra projection_entity_extra_1
          WHERE (projection_entity_extra_1.entity = entity.entity)))
UNION
 SELECT entity_to_table(entity.entity) AS projection_name,
    entity.entity,
    entity.title,
    entity_to_table(entity.base_entity) AS jump,
    entity.primarykey,
    NULL::text AS additional,
    (NOT has_table_privilege(entity.entity, 'insert, update, DELETE'::text)) AS readonly,
    (0)::bigint AS amount_dup,
    entity.base_entity,
    entity.hint,
    entity.ems_en
   FROM entity
  WHERE (NOT (EXISTS ( SELECT projection_entity_extra.projection_name
           FROM projection_entity_extra
          WHERE (projection_entity_extra.projection_name = entity_to_table(entity.entity)))));


ALTER TABLE projection_entity OWNER TO postgres;

--
-- TOC entry 275 (class 1259 OID 405815)
-- Name: projection_relation_extra; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE projection_relation_extra (
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


ALTER TABLE projection_relation_extra OWNER TO postgres;

--
-- TOC entry 276 (class 1259 OID 405822)
-- Name: relation_add; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE relation_add (
    relation_name text NOT NULL,
    relation_entity text,
    entity text,
    title text,
    key character varying,
    "order" integer,
    ems_en integer,
    hint text
);


ALTER TABLE relation_add OWNER TO postgres;

--
-- TOC entry 277 (class 1259 OID 405828)
-- Name: relation_extra; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE relation_extra (
    title text,
    ref_key text,
    relation_name text NOT NULL,
    "order" integer,
    ems_en integer,
    hint text
);


ALTER TABLE relation_extra OWNER TO postgres;

--
-- TOC entry 278 (class 1259 OID 405834)
-- Name: relation; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW relation AS
 SELECT (((((((((nr.nspname)::text || '.'::text) || (r.relname)::text) || '.'::text) || (nc.nspname)::text) || '.'::text) || (e.relname)::text) || '.'::text) || (at.attname)::text) AS relation_name,
    (((nc.nspname)::text || '.'::text) || (e.relname)::text) AS relation_entity,
    (((nr.nspname)::text || '.'::text) || (r.relname)::text) AS entity,
    COALESCE(re.title, (e.relname)::text) AS title,
    (at.attname)::character varying AS key,
    COALESCE(re."order", 0) AS "order",
    COALESCE(re.ems_en, 0) AS ems_en,
    re.hint
   FROM ((((((pg_class e
     JOIN pg_constraint c ON (((e.oid = c.conrelid) AND (c.contype = 'f'::"char"))))
     JOIN pg_namespace nc ON ((e.relnamespace = nc.oid)))
     LEFT JOIN pg_class r ON ((r.oid = c.confrelid)))
     LEFT JOIN pg_namespace nr ON ((r.relnamespace = nr.oid)))
     LEFT JOIN pg_attribute at ON (((c.conkey[1] = at.attnum) AND (at.attrelid = c.conrelid))))
     LEFT JOIN relation_extra re ON ((re.relation_name = (((((((((nr.nspname)::text || '.'::text) || (r.relname)::text) || '.'::text) || (nc.nspname)::text) || '.'::text) || (e.relname)::text) || '.'::text) || (at.attname)::text))))
UNION
 SELECT relation_add.relation_name,
    relation_add.relation_entity,
    relation_add.entity,
    relation_add.title,
    relation_add.key,
    relation_add."order",
    relation_add.ems_en,
    relation_add.hint
   FROM relation_add;


ALTER TABLE relation OWNER TO postgres;

--
-- TOC entry 279 (class 1259 OID 405839)
-- Name: projection_relation; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW projection_relation AS
 SELECT ((projection_entity.projection_name || '.'::text) || relation.relation_name) AS projection_relation_name,
    COALESCE(projection_relation_extra.title, relation.title) AS title,
    COALESCE(projection_relation_extra.related_projection_name, entity_to_table(relation.relation_entity)) AS related_projection_name,
    COALESCE(projection_relation_extra.readonly, false) AS readonly,
    COALESCE(projection_relation_extra.visible, true) AS visible,
    projection_entity.projection_name,
    relation.relation_entity,
    relation.entity,
    relation.key,
    COALESCE(projection_relation_extra.opened, false) AS opened,
    COALESCE(projection_relation_extra."order", relation."order") AS "order",
    projection_relation_extra.view_id,
    projection_relation_extra.hint
   FROM ((projection_entity
     LEFT JOIN relation ON ((relation.entity = projection_entity.entity)))
     LEFT JOIN projection_relation_extra ON ((((projection_entity.projection_name || '.'::text) || relation.relation_name) = projection_relation_extra.projection_relation_name)))
  ORDER BY COALESCE(projection_relation_extra."order", relation."order");


ALTER TABLE projection_relation OWNER TO postgres;

--
-- TOC entry 310 (class 1259 OID 406020)
-- Name: tables; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW tables AS
 SELECT tables.table_catalog,
    (((tables.table_schema)::text || '.'::text) || (tables.table_name)::text) AS entity,
    tables.table_type,
    tables.self_referencing_column_name,
    tables.reference_generation,
    tables.user_defined_type_catalog,
    tables.user_defined_type_schema,
    tables.user_defined_type_name,
    tables.is_insertable_into,
    tables.is_typed,
    tables.commit_action
   FROM information_schema.tables
  WHERE (((tables.table_schema)::text <> 'information_schema'::text) AND ((tables.table_schema)::text <> 'pg_catalog'::text));


ALTER TABLE tables OWNER TO postgres;

--
-- TOC entry 311 (class 1259 OID 406024)
-- Name: base_entity; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW base_entity AS
 SELECT tables.entity,
    tables.entity AS base_entity,
    entity.primarykey
   FROM (tables
     LEFT JOIN entity ON ((tables.entity = entity.entity)))
  WHERE ((tables.table_type)::text = 'BASE TABLE'::text)
UNION
 SELECT (((t2.view_schema)::text || '.'::text) || (t2.view_name)::text) AS entity,
    (((t2.table_schema)::text || '.'::text) || (t2.table_name)::text) AS base_entity,
    entity.primarykey
   FROM (information_schema.view_table_usage t2
     LEFT JOIN entity ON (((((t2.table_schema)::text || '.'::text) || (t2.table_name)::text) = entity.entity)));


ALTER TABLE base_entity OWNER TO postgres;

--
-- TOC entry 312 (class 1259 OID 406029)
-- Name: base_table; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW base_table AS
 SELECT entity.entity,
    entity.title
   FROM entity
  WHERE ((entity.table_type)::text = 'BASE TABLE'::text);


ALTER TABLE base_table OWNER TO postgres;

--
-- TOC entry 313 (class 1259 OID 406033)
-- Name: columns; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW columns AS
 SELECT columns.table_catalog,
    columns.table_schema,
    columns.table_name,
    (((columns.table_schema)::text || '.'::text) || (columns.table_name)::text) AS entity,
    columns.column_name,
    columns.ordinal_position,
    columns.column_default,
    columns.is_nullable,
    columns.data_type,
    columns.character_maximum_length,
    columns.character_octet_length,
    columns.numeric_precision,
    columns.numeric_precision_radix,
    columns.numeric_scale,
    columns.datetime_precision,
    columns.interval_type,
    columns.interval_precision,
    columns.character_set_catalog,
    columns.character_set_schema,
    columns.character_set_name,
    columns.collation_catalog,
    columns.collation_schema,
    columns.collation_name,
    columns.domain_catalog,
    columns.domain_schema,
    columns.domain_name,
    columns.udt_catalog,
    columns.udt_schema,
    columns.udt_name,
    columns.scope_catalog,
    columns.scope_schema,
    columns.scope_name,
    columns.maximum_cardinality,
    columns.dtd_identifier,
    columns.is_self_referencing,
    columns.is_identity,
    columns.identity_generation,
    columns.identity_start,
    columns.identity_increment,
    columns.identity_maximum,
    columns.identity_minimum,
    columns.identity_cycle,
    columns.is_generated,
    columns.generation_expression,
    columns.is_updatable
   FROM information_schema.columns
  WHERE (((columns.table_schema)::text <> 'meta'::text) AND ((columns.table_schema)::text <> 'information_schema'::text) AND ((columns.table_schema)::text <> 'pg_catalog'::text));


ALTER TABLE columns OWNER TO postgres;

--
-- TOC entry 314 (class 1259 OID 406038)
-- Name: constraint_column_usage; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW constraint_column_usage AS
 SELECT (current_database())::information_schema.sql_identifier AS table_catalog,
    (x.tblschema)::information_schema.sql_identifier AS table_schema,
    (x.tblname)::information_schema.sql_identifier AS table_name,
    (x.colname)::information_schema.sql_identifier AS column_name,
    (current_database())::information_schema.sql_identifier AS constraint_catalog,
    (x.cstrschema)::information_schema.sql_identifier AS constraint_schema,
    (x.cstrname)::information_schema.sql_identifier AS constraint_name
   FROM ( SELECT DISTINCT nr.nspname,
            r.relname,
            r.relowner,
            a.attname,
            nc.nspname,
            c.conname
           FROM pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_depend d,
            pg_namespace nc,
            pg_constraint c
          WHERE ((nr.oid = r.relnamespace) AND (r.oid = a.attrelid) AND (d.refclassid = ('pg_class'::regclass)::oid) AND (d.refobjid = r.oid) AND (d.refobjsubid = a.attnum) AND (d.classid = ('pg_constraint'::regclass)::oid) AND (d.objid = c.oid) AND (c.connamespace = nc.oid) AND (c.contype = 'c'::"char") AND (r.relkind = 'r'::"char") AND (NOT a.attisdropped))
        UNION ALL
         SELECT nr.nspname,
            r.relname,
            r.relowner,
            a.attname,
            nc.nspname,
            c.conname
           FROM pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_namespace nc,
            pg_constraint c
          WHERE ((nr.oid = r.relnamespace) AND (r.oid = a.attrelid) AND (nc.oid = c.connamespace) AND
                CASE
                    WHEN (c.contype = 'f'::"char") THEN ((r.oid = c.confrelid) AND (a.attnum = ANY (c.confkey)))
                    ELSE ((r.oid = c.conrelid) AND (a.attnum = ANY (c.conkey)))
                END AND (NOT a.attisdropped) AND (c.contype = ANY (ARRAY['p'::"char", 'u'::"char", 'f'::"char"])) AND (r.relkind = 'r'::"char"))) x(tblschema, tblname, tblowner, colname, cstrschema, cstrname);


ALTER TABLE constraint_column_usage OWNER TO postgres;

--
-- TOC entry 315 (class 1259 OID 406043)
-- Name: constraint_column_usage_ex; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW constraint_column_usage_ex AS
 SELECT (current_database())::information_schema.sql_identifier AS table_catalog,
    (x.tblschema)::information_schema.sql_identifier AS table_schema,
    (x.tblname)::information_schema.sql_identifier AS table_name,
    (x.contblschema)::information_schema.sql_identifier AS constraint_table_schema,
    (x.contblname)::information_schema.sql_identifier AS constraint_table_name,
    (x.colname)::information_schema.sql_identifier AS column_name,
    (current_database())::information_schema.sql_identifier AS constraint_catalog,
    (x.cstrschema)::information_schema.sql_identifier AS constraint_schema,
    (x.cstrname)::information_schema.sql_identifier AS constraint_name
   FROM ( SELECT DISTINCT nr.nspname,
            r.relname,
            r.relowner,
            a.attname,
            nc.nspname,
            c.conname,
            NULL::text AS text,
            NULL::text AS text
           FROM pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_depend d,
            pg_namespace nc,
            pg_constraint c
          WHERE ((nr.oid = r.relnamespace) AND (r.oid = a.attrelid) AND (d.refclassid = ('pg_class'::regclass)::oid) AND (d.refobjid = r.oid) AND (d.refobjsubid = a.attnum) AND (d.classid = ('pg_constraint'::regclass)::oid) AND (d.objid = c.oid) AND (c.connamespace = nc.oid) AND (c.contype = 'c'::"char") AND (r.relkind = 'r'::"char") AND (NOT a.attisdropped))
        UNION ALL
         SELECT nr.nspname,
            r.relname,
            r.relowner,
            a.attname,
            nc.nspname,
            c.conname,
            cnr.nspname,
            cr.relname
           FROM pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_namespace nc,
            pg_constraint c,
            pg_namespace cnr,
            pg_class cr
          WHERE ((nr.oid = r.relnamespace) AND (r.oid = a.attrelid) AND (nc.oid = c.connamespace) AND
                CASE
                    WHEN (c.contype = 'f'::"char") THEN ((r.oid = c.confrelid) AND (a.attnum = ANY (c.confkey)))
                    ELSE ((r.oid = c.conrelid) AND (a.attnum = ANY (c.conkey)))
                END AND (NOT a.attisdropped) AND (c.contype = ANY (ARRAY['p'::"char", 'u'::"char", 'f'::"char"])) AND (r.relkind = 'r'::"char") AND (cnr.oid = cr.relnamespace) AND (c.conrelid = cr.oid) AND (c.contype <> ALL (ARRAY['t'::"char", 'x'::"char"])) AND (cr.relkind = 'r'::"char") AND (NOT pg_is_other_temp_schema(nr.oid)) AND (pg_has_role(cr.relowner, 'USAGE'::text) OR has_table_privilege(cr.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR has_any_column_privilege(cr.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)))) x(tblschema, tblname, tblowner, colname, cstrschema, cstrname, contblschema, contblname);


ALTER TABLE constraint_column_usage_ex OWNER TO postgres;

--
-- TOC entry 316 (class 1259 OID 406048)
-- Name: ems_en_entity_type; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW ems_en_entity_type AS
 SELECT 0 AS type,
    'нет'::text AS note
UNION
 SELECT 1 AS type,
    'как данные'::text AS note
UNION
 SELECT 2 AS type,
    'как документ'::text AS note;


ALTER TABLE ems_en_entity_type OWNER TO postgres;

--
-- TOC entry 317 (class 1259 OID 406052)
-- Name: ems_en_relation_type; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW ems_en_relation_type AS
 SELECT 0 AS type,
    'нет'::text AS note
UNION
 SELECT 1 AS type,
    'обновлять ключи'::text AS note
UNION
 SELECT 2 AS type,
    'копировать вместе с базовой'::text AS note
UNION
 SELECT 3 AS type,
    'только проверять подписи'::text AS note;


ALTER TABLE ems_en_relation_type OWNER TO postgres;

--
-- TOC entry 318 (class 1259 OID 406056)
-- Name: entity_type; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW entity_type AS
 SELECT 'r'::text AS type,
    'Таблица'::text AS note
UNION
 SELECT 'v'::text AS type,
    'Представление'::text AS note;


ALTER TABLE entity_type OWNER TO postgres;

--
-- TOC entry 319 (class 1259 OID 406060)
-- Name: functions; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW functions AS
 SELECT p.proname AS function_name,
    p.prosrc AS function_code,
    n.nspname AS function_schema,
    (((n.nspname)::text || '.'::text) || (p.proname)::text) AS function_key
   FROM (pg_namespace n
     JOIN pg_proc p ON ((p.pronamespace = n.oid)))
  WHERE ((n.nspname <> 'information_schema'::name) AND (n.nspname <> 'pg_catalog'::name));


ALTER TABLE functions OWNER TO postgres;

--
-- TOC entry 321 (class 1259 OID 406069)
-- Name: grants; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW grants AS
 SELECT ((entity.entity || '.'::text) || (groups.groname)::text) AS key,
    entity.entity,
    groups.groname,
    COALESCE(( SELECT true AS bool
           FROM information_schema.role_table_grants
          WHERE (((((role_table_grants.table_schema)::text || '.'::text) || (role_table_grants.table_name)::text) = entity.entity) AND ((role_table_grants.grantee)::name = groups.groname) AND ((role_table_grants.privilege_type)::text = 'INSERT'::text))), false) AS insert,
    COALESCE(( SELECT true AS bool
           FROM information_schema.role_table_grants
          WHERE (((((role_table_grants.table_schema)::text || '.'::text) || (role_table_grants.table_name)::text) = entity.entity) AND ((role_table_grants.grantee)::name = groups.groname) AND ((role_table_grants.privilege_type)::text = 'SELECT'::text))), false) AS "SELECT",
    COALESCE(( SELECT true AS bool
           FROM information_schema.role_table_grants
          WHERE (((((role_table_grants.table_schema)::text || '.'::text) || (role_table_grants.table_name)::text) = entity.entity) AND ((role_table_grants.grantee)::name = groups.groname) AND ((role_table_grants.privilege_type)::text = 'DELETE'::text))), false) AS DELETE,
    COALESCE(( SELECT true AS bool
           FROM information_schema.role_table_grants
          WHERE (((((role_table_grants.table_schema)::text || '.'::text) || (role_table_grants.table_name)::text) = entity.entity) AND ((role_table_grants.grantee)::name = groups.groname) AND ((role_table_grants.privilege_type)::text = 'UPDATE'::text))), false) AS update
   FROM entity,
    system.groups;


ALTER TABLE grants OWNER TO postgres;

--
-- TOC entry 322 (class 1259 OID 406074)
-- Name: menu_item; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE menu_item (
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


ALTER TABLE menu_item OWNER TO postgres;

--
-- TOC entry 323 (class 1259 OID 406082)
-- Name: menu; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW menu AS
 WITH RECURSIVE temp1(name, parent, title, projection, view_id, role, path, level, iconclass) AS (
         SELECT t1.name,
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
           FROM menu_item t1
          WHERE (t1.parent IS NULL)
        UNION
         SELECT t2.name,
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
           FROM (menu_item t2
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
             JOIN projection_entity ON ((tin.projection = projection_entity.projection_name)))
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
     JOIN projection_entity ON ((temp1.projection = projection_entity.projection_name)))
  WHERE ((temp1.role IS NULL) OR pg_has_role("current_user"(), (temp1.role)::name, 'member'::text))
 LIMIT 1000;


ALTER TABLE menu OWNER TO postgres;

--
-- TOC entry 324 (class 1259 OID 406087)
-- Name: page; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE page (
    page_key text NOT NULL,
    title text
);


ALTER TABLE page OWNER TO postgres;

--
-- TOC entry 325 (class 1259 OID 406093)
-- Name: page_block; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE page_block (
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


ALTER TABLE page_block OWNER TO postgres;

--
-- TOC entry 326 (class 1259 OID 406100)
-- Name: page_block_layout; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW page_block_layout AS
 SELECT 0 AS layout,
    'Нет'::text AS name
UNION
 SELECT 1 AS layout,
    'Вертикальная раскладка'::text AS name
UNION
 SELECT 2 AS layout,
    'Горизонтальная раскладка'::text AS name;


ALTER TABLE page_block_layout OWNER TO postgres;

--
-- TOC entry 519 (class 1259 OID 462263)
-- Name: pivot; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE pivot (
    entity text NOT NULL,
    entity_row text,
    title_column text,
    num_column text,
    hint_column text
);


ALTER TABLE pivot OWNER TO postgres;

--
-- TOC entry 327 (class 1259 OID 406104)
-- Name: projection_buttons; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE projection_buttons (
    button text NOT NULL,
    projection_name text NOT NULL,
    title text,
    icon text,
    function text,
    schema text,
    use_in_list boolean
);


ALTER TABLE projection_buttons OWNER TO postgres;

--
-- TOC entry 328 (class 1259 OID 406110)
-- Name: projection_property_extra; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE projection_property_extra (
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


ALTER TABLE projection_property_extra OWNER TO postgres;

--
-- TOC entry 329 (class 1259 OID 406116)
-- Name: property_extra; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE property_extra (
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


ALTER TABLE property_extra OWNER TO postgres;

--
-- TOC entry 4253 (class 0 OID 0)
-- Dependencies: 329
-- Name: COLUMN property_extra.ref_key; Type: COMMENT; Schema: meta; Owner: postgres
--

COMMENT ON COLUMN property_extra.ref_key IS 'Ключевое поле в присоединяемой таблице, из которого берутся данные и подставляются в поле текущей таблице при выборе из лукапа.';


--
-- TOC entry 4254 (class 0 OID 0)
-- Dependencies: 329
-- Name: COLUMN property_extra.link_key; Type: COMMENT; Schema: meta; Owner: postgres
--

COMMENT ON COLUMN property_extra.link_key IS 'Поле текущей таблицы, из которого надо взять значение ключа для перехода по ссылке из табличного представления';


--
-- TOC entry 330 (class 1259 OID 406122)
-- Name: property; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW property AS
 SELECT (((c.relname)::text || '.'::text) || (a.attname)::text) AS property_name,
    (((nc.nspname)::text || '.'::text) || (c.relname)::text) AS entity,
    COALESCE(pe.visible, true) AS visible,
    COALESCE(pe.readonly,
        CASE
            WHEN ((c.relkind = ANY (ARRAY['f'::"char", 'p'::"char"])) OR ((c.relkind = ANY (ARRAY['v'::"char", 'r'::"char"])) AND (NOT pg_column_is_updatable((c.oid)::regclass, a.attnum, false)))) THEN true
            ELSE false
        END) AS readonly,
    COALESCE(
        CASE
            WHEN (co.conkey[1] IS NOT NULL) THEN 'ref'::text
            WHEN (a.atttypid = (2950)::oid) THEN 'invisible'::text
            ELSE NULL::text
        END, COALESCE(pe.type, 'string'::text)) AS type,
    COALESCE(pe.title,
        CASE
            WHEN (pv.* IS NOT NULL) THEN get_name_column(pv.entity, (a.attname)::text)
            ELSE (a.attname)::text
        END) AS title,
    (
        CASE
            WHEN (t.typtype = 'd'::"char") THEN
            CASE
                WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1'::integer)) THEN 'ARRAY'::text
                WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END
            ELSE (
            CASE
                WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1'::integer)) THEN 'ARRAY'::text
                WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END ||
            CASE
                WHEN (a.atttypmod = '-1'::integer) THEN ''::text
                WHEN (a.atttypid = ANY (ARRAY[(1042)::oid, (1043)::oid])) THEN (('('::text || (a.atttypmod - 4)) || ')'::text)
                WHEN (a.atttypid = ANY (ARRAY[(1560)::oid, (1562)::oid])) THEN (('('::text || a.atttypmod) || ')'::text)
                ELSE ''::text
            END)
        END)::information_schema.character_data AS data_type,
    COALESCE(pe.ref_entity, (((nr.nspname)::text || '.'::text) || (r.relname)::text)) AS ref_entity,
    (a.attname)::text AS column_name,
    (COALESCE(pe.ref_key, (at.attname)::text))::information_schema.sql_identifier AS ref_key,
    pe.ref_filter,
    pe.link_key,
    COALESCE(pe."order",
        CASE
            WHEN (pv.* IS NOT NULL) THEN get_order_column(pv.entity, (a.attname)::text)
            ELSE 999
        END) AS "order",
    COALESCE(pe.hint,
        CASE
            WHEN (pv.* IS NOT NULL) THEN get_hint_column(pv.entity, (a.attname)::text)
            ELSE NULL::text
        END) AS hint,
    (co.conname)::information_schema.sql_identifier AS constraint_name,
    (NOT (a.attnotnull OR ((t.typtype = 'd'::"char") AND t.typnotnull))) AS is_nullable,
    pg_get_expr(ad.adbin, ad.adrelid) AS "default",
    pe.pattern
   FROM ((((((pg_attribute a
     LEFT JOIN pg_attrdef ad ON (((a.attrelid = ad.adrelid) AND (a.attnum = ad.adnum))))
     JOIN (pg_class c
     JOIN pg_namespace nc ON ((c.relnamespace = nc.oid))) ON ((a.attrelid = c.oid)))
     JOIN (pg_type t
     JOIN pg_namespace nt ON ((t.typnamespace = nt.oid))) ON ((a.atttypid = t.oid)))
     LEFT JOIN property_extra pe ON ((((((nc.nspname)::text || '.'::text) || (c.relname)::text) = pe.entity) AND ((a.attname)::text = pe.column_name))))
     LEFT JOIN (pg_constraint co
     JOIN ((pg_class r
     LEFT JOIN pg_namespace nr ON ((r.relnamespace = nr.oid)))
     JOIN (pg_constraint cr
     JOIN pg_attribute at ON (((cr.conkey[1] = at.attnum) AND (at.attrelid = cr.conrelid)))) ON (((r.oid = cr.conrelid) AND (cr.contype = 'p'::"char")))) ON ((r.oid = co.confrelid))) ON (((c.oid = co.conrelid) AND (co.contype = 'f'::"char") AND (a.attnum = co.conkey[1]))))
     LEFT JOIN pivot pv ON ((pv.entity = (((nc.nspname)::text || '.'::text) || (c.relname)::text))))
  WHERE ((a.attnum > 0) AND (NOT a.attisdropped) AND (c.relkind = ANY (ARRAY['r'::"char", 'v'::"char", 'f'::"char", 'p'::"char"])) AND (pg_has_role(c.relowner, 'USAGE'::text) OR has_column_privilege(c.oid, a.attnum, 'SELECT, INSERT, UPDATE, REFERENCES'::text)) AND (nc.nspname <> ALL (ARRAY['information_schema'::name, 'pg_catalog'::name, 'meta'::name])));


ALTER TABLE property OWNER TO postgres;

--
-- TOC entry 331 (class 1259 OID 406127)
-- Name: property_add; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW property_add AS
 SELECT ((((entity.base_entity || '.~'::text) || entity.entity) || '.~'::text) || (a.attname)::text) AS property_name,
    entity.base_entity AS entity,
    false AS visible,
    true AS readonly,
    'string'::text AS type,
    (a.attname)::text AS title,
    (
        CASE
            WHEN (t.typtype = 'd'::"char") THEN
            CASE
                WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1'::integer)) THEN 'ARRAY'::text
                WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END
            ELSE (
            CASE
                WHEN ((t.typelem <> (0)::oid) AND (t.typlen = '-1'::integer)) THEN 'ARRAY'::text
                WHEN (nt.nspname = 'pg_catalog'::name) THEN format_type(a.atttypid, NULL::integer)
                ELSE 'USER-DEFINED'::text
            END ||
            CASE
                WHEN (a.atttypmod = '-1'::integer) THEN ''::text
                WHEN (a.atttypid = ANY (ARRAY[(1042)::oid, (1043)::oid])) THEN (('('::text || (a.atttypmod - 4)) || ')'::text)
                WHEN (a.atttypid = ANY (ARRAY[(1560)::oid, (1562)::oid])) THEN (('('::text || a.atttypmod) || ')'::text)
                ELSE ''::text
            END)
        END)::information_schema.character_data AS data_type,
    entity.entity AS ref_entity,
    ('~'::text || (a.attname)::text) AS column_name,
    (entity.base_entity_key)::information_schema.sql_identifier AS ref_key,
    NULL::text AS ref_filter,
    NULL::text AS link_key,
    999 AS "order",
    true AS virtual,
    (a.attname)::text AS original_column_name,
    NULL::text AS hint,
    NULL::text AS pattern,
    false AS is_nullable,
    NULL::text AS "default"
   FROM ((entity
     JOIN pg_attribute a ON ((a.attrelid = entity.entity_id)))
     JOIN (pg_type t
     JOIN pg_namespace nt ON ((t.typnamespace = nt.oid))) ON ((a.atttypid = t.oid)))
  WHERE ((a.attnum > 0) AND (NOT a.attisdropped) AND (entity.base_entity_id IS NOT NULL))
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
   FROM property;


ALTER TABLE property_add OWNER TO postgres;

--
-- TOC entry 332 (class 1259 OID 406132)
-- Name: projection_property; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW projection_property AS
 SELECT ((projection_entity.projection_name || '.'::text) || property.column_name) AS projection_property_name,
    COALESCE(projection_property_extra.title, property.title) AS title,
    COALESCE(projection_property_extra.type, property.type) AS type,
    COALESCE(projection_property_extra.readonly, property.readonly) AS readonly,
    COALESCE(projection_property_extra.visible, property.visible) AS visible,
    projection_entity.projection_name,
    property.column_name,
    property.ref_key,
    COALESCE(projection_property_extra.ref_projection, entity_to_table(property.ref_entity)) AS ref_projection,
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
   FROM ((projection_entity
     LEFT JOIN property_add property ON ((projection_entity.entity = property.entity)))
     LEFT JOIN projection_property_extra ON (((property.column_name = projection_property_extra.column_name) AND (projection_entity.projection_name = projection_property_extra.projection_name))))
  ORDER BY projection_entity.projection_name, COALESCE(projection_property_extra."order", property."order");


ALTER TABLE projection_property OWNER TO postgres;

--
-- TOC entry 333 (class 1259 OID 406137)
-- Name: projection_redirect; Type: TABLE; Schema: meta; Owner: postgres
--

CREATE TABLE projection_redirect (
    projection_name text,
    projection_redirect_to text,
    role text,
    projection_redirect text NOT NULL
);


ALTER TABLE projection_redirect OWNER TO postgres;

--
-- TOC entry 334 (class 1259 OID 406143)
-- Name: projection_relation_ex; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW projection_relation_ex AS
 SELECT ((projection_entity.projection_name || '.'::text) || relation.relation_entity) AS projection_relation_name,
    COALESCE((projection_relation_extra.title || '(*)'::text), relation.title) AS title,
    COALESCE((projection_relation_extra.related_projection_name || '(*)'::text), relation.relation_entity) AS related_projection_name,
    COALESCE(projection_relation_extra.readonly, false) AS readonly,
    COALESCE(projection_relation_extra.visible, true) AS visible,
    projection_entity.projection_name,
    relation.relation_entity,
    relation.entity,
    relation.key
   FROM (((projection_entity
     LEFT JOIN entity ON ((projection_entity.entity = entity.entity)))
     LEFT JOIN relation ON ((entity.base_entity = relation.entity)))
     LEFT JOIN projection_relation_extra ON (((relation.relation_entity = projection_relation_extra.relation_entity) AND (projection_entity.projection_name = projection_relation_extra.projection_name))));


ALTER TABLE projection_relation_ex OWNER TO postgres;

--
-- TOC entry 335 (class 1259 OID 406148)
-- Name: property_type; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW property_type AS
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


ALTER TABLE property_type OWNER TO postgres;

--
-- TOC entry 336 (class 1259 OID 406153)
-- Name: table_constraints; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW table_constraints AS
 SELECT (current_database())::information_schema.sql_identifier AS constraint_catalog,
    (nc.nspname)::information_schema.sql_identifier AS constraint_schema,
    (c.conname)::information_schema.sql_identifier AS constraint_name,
    (current_database())::information_schema.sql_identifier AS table_catalog,
    ((((nr.nspname)::information_schema.sql_identifier)::text || '.'::text) || ((r.relname)::information_schema.sql_identifier)::text) AS entity,
    (
        CASE c.contype
            WHEN 'c'::"char" THEN 'CHECK'::text
            WHEN 'f'::"char" THEN 'FOREIGN KEY'::text
            WHEN 'p'::"char" THEN 'PRIMARY KEY'::text
            WHEN 'u'::"char" THEN 'UNIQUE'::text
            ELSE NULL::text
        END)::information_schema.character_data AS constraint_type,
    (
        CASE
            WHEN c.condeferrable THEN 'YES'::text
            ELSE 'NO'::text
        END)::information_schema.yes_or_no AS is_deferrable,
    (
        CASE
            WHEN c.condeferred THEN 'YES'::text
            ELSE 'NO'::text
        END)::information_schema.yes_or_no AS initially_deferred
   FROM pg_namespace nc,
    pg_namespace nr,
    pg_constraint c,
    pg_class r
  WHERE ((nc.oid = c.connamespace) AND (nr.oid = r.relnamespace) AND (c.conrelid = r.oid) AND (c.contype <> ALL (ARRAY['t'::"char", 'x'::"char"])) AND (r.relkind = 'r'::"char") AND (NOT pg_is_other_temp_schema(nr.oid)) AND (pg_has_role(r.relowner, 'USAGE'::text) OR has_table_privilege(r.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR has_any_column_privilege(r.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)))
UNION ALL
 SELECT (current_database())::information_schema.sql_identifier AS constraint_catalog,
    (nr.nspname)::information_schema.sql_identifier AS constraint_schema,
    (((((((nr.oid)::text || '_'::text) || (r.oid)::text) || '_'::text) || (a.attnum)::text) || '_not_null'::text))::information_schema.sql_identifier AS constraint_name,
    (current_database())::information_schema.sql_identifier AS table_catalog,
    ((((nr.nspname)::information_schema.sql_identifier)::text || '.'::text) || ((r.relname)::information_schema.sql_identifier)::text) AS entity,
    ('CHECK'::character varying)::information_schema.character_data AS constraint_type,
    ('NO'::character varying)::information_schema.yes_or_no AS is_deferrable,
    ('NO'::character varying)::information_schema.yes_or_no AS initially_deferred
   FROM pg_namespace nr,
    pg_class r,
    pg_attribute a
  WHERE ((nr.oid = r.relnamespace) AND (r.oid = a.attrelid) AND a.attnotnull AND (a.attnum > 0) AND (NOT a.attisdropped) AND (r.relkind = 'r'::"char") AND (NOT pg_is_other_temp_schema(nr.oid)) AND (pg_has_role(r.relowner, 'USAGE'::text) OR has_table_privilege(r.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text) OR has_any_column_privilege(r.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)));


ALTER TABLE table_constraints OWNER TO postgres;

--
-- TOC entry 337 (class 1259 OID 406158)
-- Name: reference; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW reference AS
 SELECT (((key_column_usage.table_schema)::text || '.'::text) || (key_column_usage.table_name)::text) AS entity,
    (((constraint_column_usage.table_schema)::text || '.'::text) || (constraint_column_usage.table_name)::text) AS ref_entity,
    constraint_column_usage.column_name AS ref_key,
    key_column_usage.column_name,
    table_constraints.constraint_name
   FROM constraint_column_usage_ex constraint_column_usage,
    table_constraints,
    information_schema.key_column_usage
  WHERE (((table_constraints.constraint_name)::text = (constraint_column_usage.constraint_name)::text) AND ((table_constraints.constraint_schema)::text = (constraint_column_usage.constraint_schema)::text) AND (entity_to_table(table_constraints.entity) = (constraint_column_usage.constraint_table_name)::text) AND (entity_to_schema(table_constraints.entity) = (constraint_column_usage.constraint_table_schema)::text) AND ((key_column_usage.constraint_schema)::text = (table_constraints.constraint_schema)::text) AND ((key_column_usage.constraint_name)::text = (table_constraints.constraint_name)::text) AND ((key_column_usage.table_schema)::text = entity_to_schema(table_constraints.entity)) AND ((key_column_usage.table_name)::text = entity_to_table(table_constraints.entity)) AND ((table_constraints.constraint_type)::text = 'FOREIGN KEY'::text));


ALTER TABLE reference OWNER TO postgres;

--
-- TOC entry 338 (class 1259 OID 406163)
-- Name: schema; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW schema AS
 SELECT schemata.schema_name
   FROM information_schema.schemata
  WHERE ((schemata.schema_name)::text <> ALL (ARRAY[('pg_toast'::character varying)::text, ('pg_temp_1'::character varying)::text, ('pg_toast_temp_1'::character varying)::text, ('pg_catalog'::character varying)::text, ('information_schema'::character varying)::text]));


ALTER TABLE schema OWNER TO postgres;

--
-- TOC entry 339 (class 1259 OID 406167)
-- Name: table_relation; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW table_relation AS
 SELECT table_constraints.entity AS relation_entity,
    (((constraint_column_usage.table_schema)::text || '.'::text) || (constraint_column_usage.table_name)::text) AS entity,
    key_column_usage.table_name AS title,
    key_column_usage.column_name AS key,
    false AS many,
    table_constraints.constraint_name
   FROM ((constraint_column_usage_ex constraint_column_usage
     JOIN information_schema.key_column_usage ON ((((constraint_column_usage.constraint_schema)::text = (key_column_usage.constraint_schema)::text) AND ((constraint_column_usage.constraint_name)::text = (key_column_usage.constraint_name)::text) AND ((constraint_column_usage.constraint_table_schema)::text = (key_column_usage.table_schema)::text) AND ((constraint_column_usage.constraint_table_name)::text = (key_column_usage.table_name)::text))))
     JOIN table_constraints ON ((((table_constraints.constraint_schema)::text = (constraint_column_usage.constraint_schema)::text) AND ((table_constraints.constraint_name)::text = (constraint_column_usage.constraint_name)::text) AND ((constraint_column_usage.constraint_table_schema)::text = entity_to_schema(table_constraints.entity)) AND ((constraint_column_usage.constraint_table_name)::text = entity_to_table(table_constraints.entity)) AND ((table_constraints.constraint_type)::text = 'FOREIGN KEY'::text))))
UNION
 SELECT table_constraints.entity AS relation_entity,
    entity.entity,
    key_column_usage.table_name AS title,
    key_column_usage.column_name AS key,
    false AS many,
    table_constraints.constraint_name
   FROM (((constraint_column_usage
     JOIN information_schema.key_column_usage ON ((((constraint_column_usage.constraint_catalog)::text = (key_column_usage.constraint_catalog)::text) AND ((constraint_column_usage.constraint_schema)::text = (key_column_usage.constraint_schema)::text) AND ((constraint_column_usage.constraint_name)::text = (key_column_usage.constraint_name)::text))))
     JOIN table_constraints ON ((((table_constraints.constraint_catalog)::text = (constraint_column_usage.constraint_catalog)::text) AND ((table_constraints.constraint_schema)::text = (constraint_column_usage.constraint_schema)::text) AND ((table_constraints.constraint_name)::text = (constraint_column_usage.constraint_name)::text) AND ((table_constraints.constraint_type)::text = 'FOREIGN KEY'::text))))
     JOIN entity_extra entity ON ((entity.base_entity = (((constraint_column_usage.table_schema)::text || '.'::text) || (constraint_column_usage.table_name)::text))))
  WHERE (entity.base_entity <> entity.entity)
UNION
 SELECT r.entity AS relation_entity,
    e.entity,
    r.title,
    e.primarykey AS key,
    true AS many,
    ('_many_'::text || r.entity) AS constraint_name
   FROM (entity r
     LEFT JOIN entity e ON ((r.base_entity = e.entity)))
  WHERE (r.entity <> r.base_entity);


ALTER TABLE table_relation OWNER TO postgres;

--
-- TOC entry 340 (class 1259 OID 406172)
-- Name: user_list; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW user_list AS
 SELECT pg_authid.rolname
   FROM pg_authid;


ALTER TABLE user_list OWNER TO postgres;

--
-- TOC entry 341 (class 1259 OID 406176)
-- Name: view_entity; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_entity AS
 SELECT entity.entity,
    entity.title,
    entity.primarykey,
    entity.base_entity,
    entity.table_type,
    entity.ems_en,
    entity.hint
   FROM entity
  WHERE (entity_to_schema(entity.entity) <> ALL (ARRAY['ems'::text, 'meta'::text]));


ALTER TABLE view_entity OWNER TO postgres;

--
-- TOC entry 342 (class 1259 OID 406180)
-- Name: view_page; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_page AS
 SELECT page.page_key,
    page.title
   FROM page;


ALTER TABLE view_page OWNER TO postgres;

--
-- TOC entry 343 (class 1259 OID 406184)
-- Name: view_page_block; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_page_block AS
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
           FROM page_block t1
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
           FROM (page_block t2
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


ALTER TABLE view_page_block OWNER TO postgres;

--
-- TOC entry 344 (class 1259 OID 406189)
-- Name: view_projection_buttons; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_projection_buttons AS
 SELECT projection_buttons.button,
    projection_buttons.projection_name,
    projection_buttons.title,
    projection_buttons.icon,
    projection_buttons.function,
    projection_buttons.schema,
    projection_buttons.use_in_list
   FROM projection_buttons;


ALTER TABLE view_projection_buttons OWNER TO postgres;

--
-- TOC entry 345 (class 1259 OID 406193)
-- Name: view_projection_entity; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_projection_entity AS
 SELECT projection_entity.projection_name,
    entity_to_table(projection_entity.entity) AS table_name,
    entity_to_schema(projection_entity.entity) AS table_schema,
    projection_entity.title,
    projection_entity.jump,
    projection_entity.primarykey,
    projection_entity.additional,
    projection_entity.readonly,
    projection_entity.hint,
    projection_entity.ems_en
   FROM projection_entity;


ALTER TABLE view_projection_entity OWNER TO postgres;

--
-- TOC entry 346 (class 1259 OID 406197)
-- Name: view_projection_property; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_projection_property AS
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
   FROM projection_property;


ALTER TABLE view_projection_property OWNER TO postgres;

--
-- TOC entry 347 (class 1259 OID 406201)
-- Name: view_projection_relation; Type: VIEW; Schema: meta; Owner: postgres
--

CREATE VIEW view_projection_relation AS
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
   FROM projection_relation;


ALTER TABLE view_projection_relation OWNER TO postgres;

--
-- TOC entry 3978 (class 2606 OID 407411)
-- Name: entity_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY entity_extra
    ADD CONSTRAINT entity_extra_pkey PRIMARY KEY (entity);


--
-- TOC entry 3989 (class 2606 OID 407413)
-- Name: menu_item_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY menu_item
    ADD CONSTRAINT menu_item_pkey PRIMARY KEY (name);


--
-- TOC entry 3993 (class 2606 OID 407415)
-- Name: page_block_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY page_block
    ADD CONSTRAINT page_block_pkey PRIMARY KEY (block_key);


--
-- TOC entry 3991 (class 2606 OID 407417)
-- Name: page_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY page
    ADD CONSTRAINT page_pkey PRIMARY KEY (page_key);


--
-- TOC entry 4003 (class 2606 OID 462270)
-- Name: pivot_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY pivot
    ADD CONSTRAINT pivot_pkey PRIMARY KEY (entity);


--
-- TOC entry 3995 (class 2606 OID 407419)
-- Name: projection_buttons_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY projection_buttons
    ADD CONSTRAINT projection_buttons_pkey PRIMARY KEY (button, projection_name);


--
-- TOC entry 3980 (class 2606 OID 407421)
-- Name: projection_entity_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY projection_entity_extra
    ADD CONSTRAINT projection_entity_extra_pkey PRIMARY KEY (projection_name, entity);


--
-- TOC entry 3997 (class 2606 OID 407423)
-- Name: projection_property_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY projection_property_extra
    ADD CONSTRAINT projection_property_extra_pkey PRIMARY KEY (projection_name, column_name);


--
-- TOC entry 4001 (class 2606 OID 407425)
-- Name: projection_redirect_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY projection_redirect
    ADD CONSTRAINT projection_redirect_pkey PRIMARY KEY (projection_redirect);


--
-- TOC entry 3982 (class 2606 OID 407427)
-- Name: projection_relation_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY projection_relation_extra
    ADD CONSTRAINT projection_relation_extra_pkey PRIMARY KEY (projection_relation_name);


--
-- TOC entry 3999 (class 2606 OID 407429)
-- Name: property_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY property_extra
    ADD CONSTRAINT property_extra_pkey PRIMARY KEY (column_name, entity);


--
-- TOC entry 3984 (class 2606 OID 407431)
-- Name: relation_add_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY relation_add
    ADD CONSTRAINT relation_add_pkey PRIMARY KEY (relation_name);


--
-- TOC entry 3986 (class 2606 OID 407433)
-- Name: relation_extra_pkey; Type: CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY relation_extra
    ADD CONSTRAINT relation_extra_pkey PRIMARY KEY (relation_name);


--
-- TOC entry 3987 (class 1259 OID 407709)
-- Name: fki_menu_item_fk; Type: INDEX; Schema: meta; Owner: postgres
--

CREATE INDEX fki_menu_item_fk ON menu_item USING btree (parent);


--
-- TOC entry 4007 (class 2620 OID 407725)
-- Name: entity_DELETE_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER entity_DELETE_trg INSTEAD OF DELETE ON entity FOR EACH ROW EXECUTE PROCEDURE entity_DELETE_trgf();


--
-- TOC entry 4008 (class 2620 OID 407726)
-- Name: entity_insert_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER entity_insert_trg INSTEAD OF INSERT ON entity FOR EACH ROW EXECUTE PROCEDURE entity_insert_trgf();


--
-- TOC entry 4009 (class 2620 OID 407727)
-- Name: entity_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER entity_update_trg INSTEAD OF UPDATE ON entity FOR EACH ROW EXECUTE PROCEDURE entity_update_trgf();


--
-- TOC entry 4015 (class 2620 OID 407728)
-- Name: function_DELETE_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER function_DELETE_trg INSTEAD OF DELETE ON functions FOR EACH ROW EXECUTE PROCEDURE function_DELETE_trgf();


--
-- TOC entry 4016 (class 2620 OID 407729)
-- Name: function_insert_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER function_insert_trg INSTEAD OF INSERT ON functions FOR EACH ROW EXECUTE PROCEDURE function_insert_trgf();


--
-- TOC entry 4017 (class 2620 OID 407730)
-- Name: function_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER function_update_trg INSTEAD OF UPDATE ON functions FOR EACH ROW EXECUTE PROCEDURE function_update_trgf();


--
-- TOC entry 4018 (class 2620 OID 407731)
-- Name: grants_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER grants_update_trg INSTEAD OF UPDATE ON grants FOR EACH ROW EXECUTE PROCEDURE grants_update_trgf();


--
-- TOC entry 4019 (class 2620 OID 407732)
-- Name: menu_item_tr; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER menu_item_tr BEFORE INSERT ON menu_item FOR EACH ROW EXECUTE PROCEDURE menu_item_insert_trgf();


--
-- TOC entry 4010 (class 2620 OID 407733)
-- Name: projection_entity_DELETE_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_entity_DELETE_trg INSTEAD OF DELETE ON projection_entity FOR EACH ROW EXECUTE PROCEDURE projection_entity_DELETE_trgf();


--
-- TOC entry 4011 (class 2620 OID 407734)
-- Name: projection_entity_insert_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_entity_insert_trg INSTEAD OF INSERT ON projection_entity FOR EACH ROW EXECUTE PROCEDURE projection_entity_insert_trgf();


--
-- TOC entry 4012 (class 2620 OID 407735)
-- Name: projection_entity_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_entity_update_trg INSTEAD OF UPDATE ON projection_entity FOR EACH ROW EXECUTE PROCEDURE projection_entity_update_trgf();


--
-- TOC entry 4023 (class 2620 OID 407736)
-- Name: projection_property_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_property_update_trg INSTEAD OF INSERT OR UPDATE ON projection_property FOR EACH ROW EXECUTE PROCEDURE projection_property_update_trgf();


--
-- TOC entry 4024 (class 2620 OID 407737)
-- Name: projection_redirect_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_redirect_trg BEFORE INSERT OR UPDATE ON projection_redirect FOR EACH ROW EXECUTE PROCEDURE projection_redirect();


--
-- TOC entry 4014 (class 2620 OID 407738)
-- Name: projection_relation_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER projection_relation_update_trg INSTEAD OF INSERT OR UPDATE ON projection_relation FOR EACH ROW EXECUTE PROCEDURE projection_relation_update_trgf();


--
-- TOC entry 4020 (class 2620 OID 407739)
-- Name: property_DELETE_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER property_DELETE_trg INSTEAD OF DELETE ON property FOR EACH ROW EXECUTE PROCEDURE property_DELETE_trgf();


--
-- TOC entry 4021 (class 2620 OID 407740)
-- Name: property_insert_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER property_insert_trg INSTEAD OF INSERT ON property FOR EACH ROW EXECUTE PROCEDURE property_insert_trgf();


--
-- TOC entry 4022 (class 2620 OID 407741)
-- Name: property_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER property_update_trg INSTEAD OF UPDATE ON property FOR EACH ROW EXECUTE PROCEDURE property_update_trgf();


--
-- TOC entry 4013 (class 2620 OID 407742)
-- Name: relation_update_trg; Type: TRIGGER; Schema: meta; Owner: postgres
--

CREATE TRIGGER relation_update_trg INSTEAD OF INSERT OR UPDATE ON relation FOR EACH ROW EXECUTE PROCEDURE relation_update_trgf();


--
-- TOC entry 4004 (class 2606 OID 408021)
-- Name: menu_item_fk; Type: FK CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY menu_item
    ADD CONSTRAINT menu_item_fk FOREIGN KEY (parent) REFERENCES menu_item(name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 4005 (class 2606 OID 408026)
-- Name: page_block_page_key_fkey; Type: FK CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY page_block
    ADD CONSTRAINT page_block_page_key_fkey FOREIGN KEY (page_key) REFERENCES page(page_key);


--
-- TOC entry 4006 (class 2606 OID 408031)
-- Name: page_block_parent_block_key_fkey; Type: FK CONSTRAINT; Schema: meta; Owner: postgres
--

ALTER TABLE ONLY page_block
    ADD CONSTRAINT page_block_parent_block_key_fkey FOREIGN KEY (parent_block_key) REFERENCES page_block(block_key);


--
-- TOC entry 4225 (class 0 OID 0)
-- Dependencies: 37
-- Name: meta; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA meta FROM PUBLIC;
REVOKE ALL ON SCHEMA meta FROM postgres;
GRANT ALL ON SCHEMA meta TO postgres;
GRANT ALL ON SCHEMA meta TO devs;
GRANT USAGE ON SCHEMA meta TO PUBLIC;


--
-- TOC entry 4226 (class 0 OID 0)
-- Dependencies: 231
-- Name: entity_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE entity_extra FROM PUBLIC;
REVOKE ALL ON TABLE entity_extra FROM postgres;
GRANT ALL ON TABLE entity_extra TO postgres;
GRANT ALL ON TABLE entity_extra TO devs;


--
-- TOC entry 4227 (class 0 OID 0)
-- Dependencies: 232
-- Name: entity; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE entity FROM PUBLIC;
REVOKE ALL ON TABLE entity FROM postgres;
GRANT ALL ON TABLE entity TO postgres;
GRANT ALL ON TABLE entity TO devs;


--
-- TOC entry 4228 (class 0 OID 0)
-- Dependencies: 273
-- Name: projection_entity_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_entity_extra FROM PUBLIC;
REVOKE ALL ON TABLE projection_entity_extra FROM postgres;
GRANT ALL ON TABLE projection_entity_extra TO postgres;
GRANT ALL ON TABLE projection_entity_extra TO devs;


--
-- TOC entry 4229 (class 0 OID 0)
-- Dependencies: 274
-- Name: projection_entity; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_entity FROM PUBLIC;
REVOKE ALL ON TABLE projection_entity FROM postgres;
GRANT ALL ON TABLE projection_entity TO postgres;
GRANT ALL ON TABLE projection_entity TO devs;


--
-- TOC entry 4230 (class 0 OID 0)
-- Dependencies: 275
-- Name: projection_relation_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_relation_extra FROM PUBLIC;
REVOKE ALL ON TABLE projection_relation_extra FROM postgres;
GRANT ALL ON TABLE projection_relation_extra TO postgres;
GRANT ALL ON TABLE projection_relation_extra TO devs;


--
-- TOC entry 4231 (class 0 OID 0)
-- Dependencies: 276
-- Name: relation_add; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE relation_add FROM PUBLIC;
REVOKE ALL ON TABLE relation_add FROM postgres;
GRANT ALL ON TABLE relation_add TO postgres;
GRANT ALL ON TABLE relation_add TO devs;


--
-- TOC entry 4232 (class 0 OID 0)
-- Dependencies: 277
-- Name: relation_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE relation_extra FROM PUBLIC;
REVOKE ALL ON TABLE relation_extra FROM postgres;
GRANT ALL ON TABLE relation_extra TO postgres;
GRANT ALL ON TABLE relation_extra TO devs;


--
-- TOC entry 4233 (class 0 OID 0)
-- Dependencies: 278
-- Name: relation; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE relation FROM PUBLIC;
REVOKE ALL ON TABLE relation FROM postgres;
GRANT ALL ON TABLE relation TO postgres;
GRANT ALL ON TABLE relation TO devs;


--
-- TOC entry 4234 (class 0 OID 0)
-- Dependencies: 279
-- Name: projection_relation; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_relation FROM PUBLIC;
REVOKE ALL ON TABLE projection_relation FROM postgres;
GRANT ALL ON TABLE projection_relation TO postgres;
GRANT ALL ON TABLE projection_relation TO devs;


--
-- TOC entry 4235 (class 0 OID 0)
-- Dependencies: 310
-- Name: tables; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE tables FROM PUBLIC;
REVOKE ALL ON TABLE tables FROM postgres;
GRANT ALL ON TABLE tables TO postgres;
GRANT ALL ON TABLE tables TO devs;


--
-- TOC entry 4236 (class 0 OID 0)
-- Dependencies: 311
-- Name: base_entity; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE base_entity FROM PUBLIC;
REVOKE ALL ON TABLE base_entity FROM postgres;
GRANT ALL ON TABLE base_entity TO postgres;
GRANT ALL ON TABLE base_entity TO devs;


--
-- TOC entry 4237 (class 0 OID 0)
-- Dependencies: 312
-- Name: base_table; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE base_table FROM PUBLIC;
REVOKE ALL ON TABLE base_table FROM postgres;
GRANT ALL ON TABLE base_table TO postgres;
GRANT ALL ON TABLE base_table TO devs;


--
-- TOC entry 4238 (class 0 OID 0)
-- Dependencies: 313
-- Name: columns; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE columns FROM PUBLIC;
REVOKE ALL ON TABLE columns FROM postgres;
GRANT ALL ON TABLE columns TO postgres;
GRANT ALL ON TABLE columns TO devs;


--
-- TOC entry 4239 (class 0 OID 0)
-- Dependencies: 314
-- Name: constraint_column_usage; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE constraint_column_usage FROM PUBLIC;
REVOKE ALL ON TABLE constraint_column_usage FROM postgres;
GRANT ALL ON TABLE constraint_column_usage TO postgres;
GRANT ALL ON TABLE constraint_column_usage TO devs;


--
-- TOC entry 4240 (class 0 OID 0)
-- Dependencies: 315
-- Name: constraint_column_usage_ex; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE constraint_column_usage_ex FROM PUBLIC;
REVOKE ALL ON TABLE constraint_column_usage_ex FROM postgres;
GRANT ALL ON TABLE constraint_column_usage_ex TO postgres;
GRANT ALL ON TABLE constraint_column_usage_ex TO devs;


--
-- TOC entry 4241 (class 0 OID 0)
-- Dependencies: 316
-- Name: ems_en_entity_type; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE ems_en_entity_type FROM PUBLIC;
REVOKE ALL ON TABLE ems_en_entity_type FROM postgres;
GRANT ALL ON TABLE ems_en_entity_type TO postgres;
GRANT ALL ON TABLE ems_en_entity_type TO devs;


--
-- TOC entry 4242 (class 0 OID 0)
-- Dependencies: 317
-- Name: ems_en_relation_type; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE ems_en_relation_type FROM PUBLIC;
REVOKE ALL ON TABLE ems_en_relation_type FROM postgres;
GRANT ALL ON TABLE ems_en_relation_type TO postgres;
GRANT ALL ON TABLE ems_en_relation_type TO devs;


--
-- TOC entry 4243 (class 0 OID 0)
-- Dependencies: 318
-- Name: entity_type; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE entity_type FROM PUBLIC;
REVOKE ALL ON TABLE entity_type FROM postgres;
GRANT ALL ON TABLE entity_type TO postgres;
GRANT ALL ON TABLE entity_type TO devs;


--
-- TOC entry 4244 (class 0 OID 0)
-- Dependencies: 319
-- Name: functions; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE functions FROM PUBLIC;
REVOKE ALL ON TABLE functions FROM postgres;
GRANT ALL ON TABLE functions TO postgres;
GRANT ALL ON TABLE functions TO devs;


--
-- TOC entry 4245 (class 0 OID 0)
-- Dependencies: 321
-- Name: grants; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE grants FROM PUBLIC;
REVOKE ALL ON TABLE grants FROM postgres;
GRANT ALL ON TABLE grants TO postgres;
GRANT ALL ON TABLE grants TO devs;


--
-- TOC entry 4246 (class 0 OID 0)
-- Dependencies: 322
-- Name: menu_item; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE menu_item FROM PUBLIC;
REVOKE ALL ON TABLE menu_item FROM postgres;
GRANT ALL ON TABLE menu_item TO postgres;
GRANT ALL ON TABLE menu_item TO devs;


--
-- TOC entry 4247 (class 0 OID 0)
-- Dependencies: 323
-- Name: menu; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE menu FROM PUBLIC;
REVOKE ALL ON TABLE menu FROM postgres;
GRANT ALL ON TABLE menu TO postgres;
GRANT ALL ON TABLE menu TO devs;
GRANT SELECT ON TABLE menu TO PUBLIC;


--
-- TOC entry 4248 (class 0 OID 0)
-- Dependencies: 324
-- Name: page; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE page FROM PUBLIC;
REVOKE ALL ON TABLE page FROM postgres;
GRANT ALL ON TABLE page TO postgres;
GRANT ALL ON TABLE page TO devs;


--
-- TOC entry 4249 (class 0 OID 0)
-- Dependencies: 325
-- Name: page_block; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE page_block FROM PUBLIC;
REVOKE ALL ON TABLE page_block FROM postgres;
GRANT ALL ON TABLE page_block TO postgres;
GRANT ALL ON TABLE page_block TO devs;


--
-- TOC entry 4250 (class 0 OID 0)
-- Dependencies: 326
-- Name: page_block_layout; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE page_block_layout FROM PUBLIC;
REVOKE ALL ON TABLE page_block_layout FROM postgres;
GRANT ALL ON TABLE page_block_layout TO postgres;
GRANT ALL ON TABLE page_block_layout TO devs;


--
-- TOC entry 4251 (class 0 OID 0)
-- Dependencies: 327
-- Name: projection_buttons; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_buttons FROM PUBLIC;
REVOKE ALL ON TABLE projection_buttons FROM postgres;
GRANT ALL ON TABLE projection_buttons TO postgres;
GRANT ALL ON TABLE projection_buttons TO devs;


--
-- TOC entry 4252 (class 0 OID 0)
-- Dependencies: 328
-- Name: projection_property_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_property_extra FROM PUBLIC;
REVOKE ALL ON TABLE projection_property_extra FROM postgres;
GRANT ALL ON TABLE projection_property_extra TO postgres;
GRANT ALL ON TABLE projection_property_extra TO devs;


--
-- TOC entry 4255 (class 0 OID 0)
-- Dependencies: 329
-- Name: property_extra; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE property_extra FROM PUBLIC;
REVOKE ALL ON TABLE property_extra FROM postgres;
GRANT ALL ON TABLE property_extra TO postgres;
GRANT ALL ON TABLE property_extra TO devs;


--
-- TOC entry 4256 (class 0 OID 0)
-- Dependencies: 330
-- Name: property; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE property FROM PUBLIC;
REVOKE ALL ON TABLE property FROM postgres;
GRANT ALL ON TABLE property TO postgres;
GRANT ALL ON TABLE property TO devs;


--
-- TOC entry 4257 (class 0 OID 0)
-- Dependencies: 331
-- Name: property_add; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE property_add FROM PUBLIC;
REVOKE ALL ON TABLE property_add FROM postgres;
GRANT ALL ON TABLE property_add TO postgres;
GRANT ALL ON TABLE property_add TO devs;


--
-- TOC entry 4258 (class 0 OID 0)
-- Dependencies: 332
-- Name: projection_property; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_property FROM PUBLIC;
REVOKE ALL ON TABLE projection_property FROM postgres;
GRANT ALL ON TABLE projection_property TO postgres;
GRANT ALL ON TABLE projection_property TO devs;


--
-- TOC entry 4259 (class 0 OID 0)
-- Dependencies: 333
-- Name: projection_redirect; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_redirect FROM PUBLIC;
REVOKE ALL ON TABLE projection_redirect FROM postgres;
GRANT ALL ON TABLE projection_redirect TO postgres;
GRANT ALL ON TABLE projection_redirect TO devs;


--
-- TOC entry 4260 (class 0 OID 0)
-- Dependencies: 334
-- Name: projection_relation_ex; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE projection_relation_ex FROM PUBLIC;
REVOKE ALL ON TABLE projection_relation_ex FROM postgres;
GRANT ALL ON TABLE projection_relation_ex TO postgres;
GRANT ALL ON TABLE projection_relation_ex TO devs;


--
-- TOC entry 4261 (class 0 OID 0)
-- Dependencies: 335
-- Name: property_type; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE property_type FROM PUBLIC;
REVOKE ALL ON TABLE property_type FROM postgres;
GRANT ALL ON TABLE property_type TO postgres;
GRANT ALL ON TABLE property_type TO devs;


--
-- TOC entry 4262 (class 0 OID 0)
-- Dependencies: 336
-- Name: table_constraints; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE table_constraints FROM PUBLIC;
REVOKE ALL ON TABLE table_constraints FROM postgres;
GRANT ALL ON TABLE table_constraints TO postgres;
GRANT ALL ON TABLE table_constraints TO devs;


--
-- TOC entry 4263 (class 0 OID 0)
-- Dependencies: 337
-- Name: reference; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE reference FROM PUBLIC;
REVOKE ALL ON TABLE reference FROM postgres;
GRANT ALL ON TABLE reference TO postgres;
GRANT ALL ON TABLE reference TO devs;


--
-- TOC entry 4264 (class 0 OID 0)
-- Dependencies: 338
-- Name: schema; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE schema FROM PUBLIC;
REVOKE ALL ON TABLE schema FROM postgres;
GRANT ALL ON TABLE schema TO postgres;
GRANT ALL ON TABLE schema TO devs;


--
-- TOC entry 4265 (class 0 OID 0)
-- Dependencies: 339
-- Name: table_relation; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE table_relation FROM PUBLIC;
REVOKE ALL ON TABLE table_relation FROM postgres;
GRANT ALL ON TABLE table_relation TO postgres;
GRANT ALL ON TABLE table_relation TO devs;


--
-- TOC entry 4266 (class 0 OID 0)
-- Dependencies: 340
-- Name: user_list; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE user_list FROM PUBLIC;
REVOKE ALL ON TABLE user_list FROM postgres;
GRANT ALL ON TABLE user_list TO postgres;
GRANT ALL ON TABLE user_list TO devs;


--
-- TOC entry 4267 (class 0 OID 0)
-- Dependencies: 341
-- Name: view_entity; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_entity FROM PUBLIC;
REVOKE ALL ON TABLE view_entity FROM postgres;
GRANT ALL ON TABLE view_entity TO postgres;
GRANT ALL ON TABLE view_entity TO devs;


--
-- TOC entry 4268 (class 0 OID 0)
-- Dependencies: 342
-- Name: view_page; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_page FROM PUBLIC;
REVOKE ALL ON TABLE view_page FROM postgres;
GRANT ALL ON TABLE view_page TO postgres;
GRANT ALL ON TABLE view_page TO devs;
GRANT SELECT ON TABLE view_page TO PUBLIC;


--
-- TOC entry 4269 (class 0 OID 0)
-- Dependencies: 343
-- Name: view_page_block; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_page_block FROM PUBLIC;
REVOKE ALL ON TABLE view_page_block FROM postgres;
GRANT ALL ON TABLE view_page_block TO postgres;
GRANT ALL ON TABLE view_page_block TO devs;
GRANT SELECT ON TABLE view_page_block TO PUBLIC;


--
-- TOC entry 4270 (class 0 OID 0)
-- Dependencies: 344
-- Name: view_projection_buttons; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_projection_buttons FROM PUBLIC;
REVOKE ALL ON TABLE view_projection_buttons FROM postgres;
GRANT ALL ON TABLE view_projection_buttons TO postgres;
GRANT ALL ON TABLE view_projection_buttons TO devs;
GRANT SELECT ON TABLE view_projection_buttons TO PUBLIC;


--
-- TOC entry 4271 (class 0 OID 0)
-- Dependencies: 345
-- Name: view_projection_entity; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_projection_entity FROM PUBLIC;
REVOKE ALL ON TABLE view_projection_entity FROM postgres;
GRANT ALL ON TABLE view_projection_entity TO postgres;
GRANT ALL ON TABLE view_projection_entity TO devs;
GRANT SELECT ON TABLE view_projection_entity TO PUBLIC;


--
-- TOC entry 4272 (class 0 OID 0)
-- Dependencies: 346
-- Name: view_projection_property; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_projection_property FROM PUBLIC;
REVOKE ALL ON TABLE view_projection_property FROM postgres;
GRANT ALL ON TABLE view_projection_property TO postgres;
GRANT ALL ON TABLE view_projection_property TO devs;
GRANT SELECT ON TABLE view_projection_property TO PUBLIC;


--
-- TOC entry 4273 (class 0 OID 0)
-- Dependencies: 347
-- Name: view_projection_relation; Type: ACL; Schema: meta; Owner: postgres
--

REVOKE ALL ON TABLE view_projection_relation FROM PUBLIC;
REVOKE ALL ON TABLE view_projection_relation FROM postgres;
GRANT ALL ON TABLE view_projection_relation TO postgres;
GRANT ALL ON TABLE view_projection_relation TO devs;
GRANT SELECT ON TABLE view_projection_relation TO PUBLIC;


-- Completed on 2018-03-24 18:57:12

--
-- PostgreSQL database dump complete
--

