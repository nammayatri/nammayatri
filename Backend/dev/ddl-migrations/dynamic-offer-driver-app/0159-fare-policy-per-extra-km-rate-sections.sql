-- NOTE: dont need to run these queries
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  per_extra_km_rate numeric (30,2) NOT NULL,
  CONSTRAINT fare_policy_progressive_details_per_extra_km_rate_section_unique_start_distance UNIQUE (fare_policy_id, start_distance)
);
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section OWNER TO atlas_driver_offer_bpp_user;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ALTER COLUMN per_extra_km_fare DROP NOT NULL;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details DROP COLUMN IF EXISTS per_extra_km_fare;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
