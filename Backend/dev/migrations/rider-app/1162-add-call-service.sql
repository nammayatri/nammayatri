ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_exophone character varying (255) DEFAULT 'Exotel' NOT NULL;

BEGIN;  --should get executed in one transaction
UPDATE atlas_app.exophone SET call_service = 'Knowlarity' where primary_phone = '8035471715';
COMMIT;
