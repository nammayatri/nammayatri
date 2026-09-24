-- NOTE: dont need to run these queries

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN estimated_fare SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
  ALTER COLUMN base_fare SET DATA TYPE integer
  USING round(base_fare);
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN base_distance TO base_distance_meters;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

-- is the rounding necessary?
ALTER TABLE atlas_driver_offer_bpp.booking
  ALTER COLUMN estimated_distance SET DATA TYPE integer
  USING round(estimated_distance);

ALTER TABLE atlas_driver_offer_bpp.driver_quote
  ALTER COLUMN distance SET DATA TYPE integer
  USING round(distance);
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN base_fare SET DATA TYPE integer
  USING round(base_fare);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN extra_km_fare SET DATA TYPE integer
  USING round(extra_km_fare);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN driver_selected_fare SET DATA TYPE integer
  USING round(driver_selected_fare);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN extra_km_fare TO per_extra_km_fare;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy
  ALTER COLUMN base_distance_meters SET DATA TYPE integer
  USING round(base_distance_meters);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_policy
  ALTER COLUMN dead_km_fare SET DATA TYPE integer
  USING round(dead_km_fare);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

ALTER TABLE atlas_driver_offer_bpp.ride
  ALTER COLUMN fare SET DATA TYPE integer
  USING round(fare);

-- ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
--   ALTER COLUMN distance SET DATA TYPE integer
--   USING round(distance);

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS driver_min_extra_fee integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS driver_max_extra_fee integer;

ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN IF EXISTS driver_extra_fee_list;
-- NOTE: dont need to run these queries
