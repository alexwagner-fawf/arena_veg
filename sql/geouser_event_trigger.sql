-- Optional: keep geouser's read access when Arena drops and recreates a survey's
-- data schema on republish (which would lose the grants from geouser_grants.sql).
--
-- Requires a superuser (event triggers). The DDL runs as arena, which owns the new
-- objects and may therefore grant on them.
-- Keep the survey list in the regex in sync with geouser_grants.sql.

CREATE OR REPLACE FUNCTION public.arena_geouser_regrant() RETURNS event_trigger
LANGUAGE plpgsql AS $$
DECLARE
  obj record;
  pattern constant text := '^survey_(31|33)_data$';
BEGIN
  FOR obj IN SELECT * FROM pg_event_trigger_ddl_commands() LOOP
    IF obj.command_tag = 'CREATE SCHEMA' AND obj.object_identity ~ pattern THEN
      EXECUTE format('GRANT USAGE ON SCHEMA %s TO geouser', obj.object_identity);
      EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %s GRANT SELECT ON TABLES TO geouser',
                     obj.object_identity);
    ELSIF obj.command_tag IN ('CREATE TABLE', 'CREATE VIEW') AND obj.schema_name ~ pattern THEN
      EXECUTE format('GRANT SELECT ON %s TO geouser', obj.object_identity);
    END IF;
  END LOOP;
END
$$;

DROP EVENT TRIGGER IF EXISTS arena_geouser_regrant;
CREATE EVENT TRIGGER arena_geouser_regrant ON ddl_command_end
  WHEN TAG IN ('CREATE SCHEMA', 'CREATE TABLE', 'CREATE VIEW')
  EXECUTE FUNCTION public.arena_geouser_regrant();
