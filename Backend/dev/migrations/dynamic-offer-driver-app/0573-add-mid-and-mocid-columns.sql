ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN merchant_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN merchant_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.special_location ADD COLUMN merchant_id varchar(36);

ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN merchant_operating_city_id varchar(36) ;
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN merchant_id varchar(36) ;