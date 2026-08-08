CREATE TABLE atlas_driver_offer_bpp.frfs_driver_rating ();

ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN driver_rating_value integer ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN feedback_details text ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN fleet_number text ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN fleet_rating_value integer ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN gtfs_id text ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN operator_badge_token text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_driver_rating ADD PRIMARY KEY ( id);
