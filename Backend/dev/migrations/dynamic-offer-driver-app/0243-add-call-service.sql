ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN get_exophone character varying (255) DEFAULT 'Exotel' NOT NULL;

BEGIN;--should get executed in one transaction
ALTER TABLE atlas_driver_offer_bpp.exophone ADD COLUMN call_service character varying (255) DEFAULT 'Exotel' NOT NULL;
UPDATE atlas_driver_offer_bpp.exophone SET call_service = 'Knowlarity' where primary_phone = '8035471774';
COMMIT;
