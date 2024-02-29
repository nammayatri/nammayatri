CREATE TABLE atlas_driver_offer_bpp.daily_stats ();

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN merchant_local_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN num_rides integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_earnings integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD PRIMARY KEY ( id, driver_id);