-- UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
-- SET driver_background_verification_service = 'SafetyPortal';


INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT
  m.id,
  moc.id,
  'DriverBackgroundVerification_SafetyPortal',
  json_build_object(
    'url', 'https://external.beta.beckn.uat.juspay.net/api/dev/safety/v2/',
    'token', '0.1.0|1|ulhKfCaTGXPqbYx7S6mwbWS7hyS8UcaE3GnYxV3HcGeH8SUl6XdzprfqRiPZtl0ymuO+ZUhm4qcjaoPKtNpDzMrjIA7HY1rdiKP9feW+gAr1nj/zjSh+',
    'safetyWebhookAuthToken' , '0.1.0|1|ulhKfCaTGXPqbYx7S6mwbWS7hyS8UcaE3GnYxV3HcGeH8SUl6XdzprfqRiPZtl0ymuO+ZUhm4qcjaoPKtNpDzMrjIA7HY1rdiKP9feW+gAr1nj/zjSh+'
  )
FROM
  atlas_driver_offer_bpp.merchant m
JOIN
  atlas_driver_offer_bpp.merchant_operating_city moc
ON
  m.id = moc.merchant_id;

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