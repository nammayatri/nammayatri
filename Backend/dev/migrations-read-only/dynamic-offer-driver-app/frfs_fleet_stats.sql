CREATE TABLE atlas_driver_offer_bpp.frfs_fleet_stats ();

ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN fleet_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN gtfs_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN rating text ;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN total_rating_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN total_rating_score integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.frfs_fleet_stats ADD CONSTRAINT frfs_fleet_stats_unique_idx_fleet_number_gtfs_id UNIQUE (fleet_number, gtfs_id);