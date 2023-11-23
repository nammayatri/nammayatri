ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN merchant_id CHARACTER (36);
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_error text;
ALTER TABLE atlas_driver_offer_bpp.call_status ADD COLUMN call_service text;

ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id DROP NOT NULL;