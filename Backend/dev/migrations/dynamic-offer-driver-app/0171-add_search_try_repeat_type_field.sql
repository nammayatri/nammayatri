
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN search_repeat_type character varying(255);
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'INITIAL' WHERE T1.search_repeat_counter = 0;
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET search_repeat_type = 'REALLOCATION' WHERE T1.search_repeat_counter IS NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN search_repeat_type SET NOT NULL;