CREATE TABLE atlas_driver_offer_bpp.frfs_fleet_rating ();

ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN fleet_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN gtfs_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN rating text ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN total_rating_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN total_rating_score integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_rating ADD PRIMARY KEY ( id);
