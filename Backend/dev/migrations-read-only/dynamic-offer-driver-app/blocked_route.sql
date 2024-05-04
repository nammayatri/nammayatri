CREATE TABLE atlas_driver_offer_bpp.blocked_route ();

ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN end_segment text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN start_segment text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.blocked_route ADD PRIMARY KEY ( id);