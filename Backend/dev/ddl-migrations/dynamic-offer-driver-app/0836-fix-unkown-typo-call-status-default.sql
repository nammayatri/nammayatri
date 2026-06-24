ALTER TABLE atlas_driver_offer_bpp.call_status ALTER COLUMN entity_id SET DEFAULT 'UNKNOWN';
UPDATE atlas_driver_offer_bpp.call_status SET entity_id = 'UNKNOWN' WHERE entity_id = 'UNKOWN';
