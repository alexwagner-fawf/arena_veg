-- Read-only access for the Shiny apps (role geouser) to the Arena surveys they use.
--
-- Run as a superuser (or as arena): ALTER DEFAULT PRIVILEGES FOR ROLE arena needs
-- membership in arena. Idempotent: safe to re-run, e.g. after republishing a survey.
--
-- Arena rebuilds the survey_N_data tables when a survey is republished. The default
-- privileges below re-grant SELECT on those new tables automatically, as long as the
-- schema itself survives. If Arena drops and recreates the schema, re-run this script
-- or install sql/geouser_event_trigger.sql.

DO $$
DECLARE
  -- Surveys the apps read. 31 + 33 = vegetation / regeneration survey.
  survey_ids int[] := '{31,33}';
  meta_tables text[] := '{file,node_def,category,category_level,category_item}';
  id int;
  t text;
BEGIN
  GRANT USAGE ON SCHEMA public TO geouser;
  GRANT SELECT ON public.survey TO geouser;

  FOREACH id IN ARRAY survey_ids LOOP
    IF to_regnamespace('survey_' || id) IS NULL OR to_regnamespace('survey_' || id || '_data') IS NULL THEN
      RAISE NOTICE 'Survey % skipped: schema survey_% or survey_%_data does not exist', id, id, id;
      CONTINUE;
    END IF;
    EXECUTE format('GRANT USAGE ON SCHEMA %I, %I TO geouser', 'survey_' || id, 'survey_' || id || '_data');
    FOREACH t IN ARRAY meta_tables LOOP
      EXECUTE format('GRANT SELECT ON %I.%I TO geouser', 'survey_' || id, t);
    END LOOP;
    EXECUTE format('GRANT SELECT ON ALL TABLES IN SCHEMA %I TO geouser', 'survey_' || id || '_data');
    EXECUTE format('ALTER DEFAULT PRIVILEGES FOR ROLE arena IN SCHEMA %I GRANT SELECT ON TABLES TO geouser',
                   'survey_' || id || '_data');
  END LOOP;
END
$$;
