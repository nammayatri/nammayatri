CREATE TABLE atlas_app.gtfs_feed_info ();

ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN feed_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.gtfs_feed_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.gtfs_feed_info ADD PRIMARY KEY ( feed_id);
