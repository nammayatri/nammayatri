CREATE TABLE atlas_app.frfs_config ();

ALTER TABLE atlas_app.frfs_config ADD COLUMN booking_end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN booking_start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN custom_dates text[] NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN custom_end_time text NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN discount integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN metro_station_ttl integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN one_way_ticket_limit integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN round_trip_ticket_limit integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_config ADD PRIMARY KEY ( merchant_operating_city_id);