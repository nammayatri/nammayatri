-- NOTE: dont need to run these queries

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
DROP CONSTRAINT fare_policy_rental_details_pricing_slabs_pkey;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
DROP COLUMN IF EXISTS id;
DO $$ BEGIN


ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
ADD PRIMARY KEY (fare_policy_id, time_percentage, distance_percentage);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
DROP CONSTRAINT fare_policy_inter_city_details_pricing_slabs_pkey;


ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
DROP COLUMN IF EXISTS id;
DO $$ BEGIN


ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
ADD PRIMARY KEY (fare_policy_id, time_percentage, distance_percentage);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
