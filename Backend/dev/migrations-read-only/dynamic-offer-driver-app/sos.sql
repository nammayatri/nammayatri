CREATE TABLE atlas_driver_offer_bpp.sos ();

ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN flow text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN ticket_id text ;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.sos ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.sos ADD PRIMARY KEY ( id);