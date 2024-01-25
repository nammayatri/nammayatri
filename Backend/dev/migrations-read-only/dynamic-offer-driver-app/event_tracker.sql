CREATE TABLE atlas_driver_offer_bpp.event_tracker ();

ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN entity text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN entity_field_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN entity_primary_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN event_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN from_state text ;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN reason text ;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN to_state text ;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.event_tracker ADD COLUMN subscription_service_name text ;