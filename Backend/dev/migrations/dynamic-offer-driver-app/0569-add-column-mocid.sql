ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN merchant_operating_city_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN merchant_operating_city_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN merchant_operating_city_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.driver_location ADD COLUMN merchant_operating_city_id character varying(36) ;