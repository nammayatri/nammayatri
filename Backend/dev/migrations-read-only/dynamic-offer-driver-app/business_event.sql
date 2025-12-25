CREATE TABLE atlas_driver_offer_bpp.business_event ();

ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN booking_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN distance double precision ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN duration double precision ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN event_type character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN time_stamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN vehicle_variant character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN when_pool_was_computed character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.business_event ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.business_event ADD COLUMN distance_unit character varying(255) ;