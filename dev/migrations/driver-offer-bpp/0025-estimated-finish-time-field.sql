ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimated_finish_time timestamp with time zone;
UPDATE atlas_driver_offer_bpp.search_request AS T1 SET estimated_finish_time = T1.start_time;
-- Technically it ^^^ is not correct, but previously start_time had been used to count totalFare,
-- but now estimated_finish_time used instead, so i made estimated_finish_time = start_time for old rides.
ALTER TABLE atlas_driver_offer_bpp.search_request
  ALTER COLUMN estimated_finish_time SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_finish_time timestamp with time zone;
UPDATE atlas_driver_offer_bpp.booking AS T1 SET estimated_finish_time = T1.start_time;
ALTER TABLE atlas_driver_offer_bpp.booking
  ALTER COLUMN estimated_finish_time SET NOT NULL;

