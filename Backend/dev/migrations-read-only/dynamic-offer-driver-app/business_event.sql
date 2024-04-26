CREATE TABLE atlas_driver_offer_bpp.business_event ();

ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN booking_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN distance integer ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN duration integer ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN event_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN time_stamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN when_pool_was_computed text ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.business_event ALTER COLUMN updated_at DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ALTER COLUMN created_at DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.business_event DROP COLUMN merchant_operating_city_id;
ALTER TABLE atlas_driver_offer_bpp.business_event DROP COLUMN merchant_id;