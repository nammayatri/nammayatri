ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN driver_background_verification_service character varying(30);
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config
ALTER COLUMN service_name TYPE VARCHAR(50);

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET driver_background_verification_service = 'SafetyPortal';
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT
  m.id,
  moc.id,
  'DriverBackgroundVerification_SafetyPortal',
  json_build_object(
    'url', 'https://external.beta.beckn.uat.juspay.net/api/dev/safety/v2/',
    'token', '0.1.0|2|lZW9sforAur/mEaV7oaguUvPo0F0Bgw32vn49Rc5yHclgoePJKPKfT/o50tJap+geLjNIXQxJr5+Qzxq+HkZmwZgnsfSJamyTbsWM8VU3vJYMMca9jm1'
  )
FROM
  atlas_driver_offer_bpp.merchant m
JOIN
  atlas_driver_offer_bpp.merchant_operating_city moc
ON
  m.id = moc.merchant_id;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN safety_webhook_auth_token text; -- This query already ran in master
ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs ADD COLUMN dl_number_verification bool ;





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