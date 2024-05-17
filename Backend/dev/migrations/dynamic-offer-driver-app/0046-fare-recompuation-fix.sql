ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimated_distance integer;

ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN estimated_distance SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver DROP COLUMN distance;

ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN estimated_duration TYPE integer;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN estimated_duration TYPE integer;

UPDATE atlas_driver_offer_bpp.booking SET estimated_duration = estimated_distance / 16.6667; --60 km/h
UPDATE atlas_driver_offer_bpp.search_request SET estimated_duration = estimated_distance / 16.6667; --60 km/h
ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN estimated_duration SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN estimated_duration SET NOT NULL;
