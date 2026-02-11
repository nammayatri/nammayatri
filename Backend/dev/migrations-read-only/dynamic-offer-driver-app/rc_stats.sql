CREATE TABLE atlas_driver_offer_bpp.rc_stats ();

ALTER TABLE atlas_driver_offer_bpp.rc_stats ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rc_stats ADD COLUMN total_rides integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.rc_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rc_stats ADD PRIMARY KEY ( rc_id);
