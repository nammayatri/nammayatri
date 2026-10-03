-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS merchant_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS merchant_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.special_location ADD COLUMN merchant_id varchar(36);-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
