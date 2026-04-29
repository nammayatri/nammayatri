

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN safety_webhook_auth_token text; -- This query already ran in master

------------------------------------------------------------------------------------------------------
-------------- This query is only for local  do not run in master or prod environment ----------------
------------------------------------------------------------------------------------------------------
DO $$
 DECLARE
     sql_query TEXT;
     schema_name TEXT := 'atlas_driver_offer_bpp';
     constraint_data RECORD;
 BEGIN
     FOR constraint_data IN
         SELECT conname, conrelid::regclass AS table_name
         FROM pg_constraint
         WHERE connamespace = schema_name::regnamespace AND contype = 'f'
     LOOP
         BEGIN
             EXECUTE 'ALTER TABLE ' || (constraint_data.table_name::text) ||
                     ' DROP CONSTRAINT ' || (constraint_data.conname);
             RAISE NOTICE 'Dropped constraint: %', constraint_data.conname;
         EXCEPTION
             WHEN others THEN
                 RAISE NOTICE 'Error dropping constraint %: from table: %', constraint_data.conname, constraint_data.table_name;
         END;
     END LOOP;
 END $$;