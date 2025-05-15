CREATE TABLE atlas_app.frfs_config ();

ALTER TABLE atlas_app.frfs_config ADD COLUMN booking_end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN booking_start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN bus_station_ttl integer  default 1800;
ALTER TABLE atlas_app.frfs_config ADD COLUMN cancellation_reason_id text ;
ALTER TABLE atlas_app.frfs_config ADD COLUMN custom_dates text[] NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN custom_end_time text NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN discount integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN free_ticket_interval integer  default 9999;
ALTER TABLE atlas_app.frfs_config ADD COLUMN is_cancellation_allowed boolean  default true;
ALTER TABLE atlas_app.frfs_config ADD COLUMN is_event_ongoing boolean  default false;
ALTER TABLE atlas_app.frfs_config ADD COLUMN max_free_ticket_cashback integer  default 0;
ALTER TABLE atlas_app.frfs_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN metro_station_ttl integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN one_way_ticket_limit integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN provider_id text ;
ALTER TABLE atlas_app.frfs_config ADD COLUMN provider_name text ;
ALTER TABLE atlas_app.frfs_config ADD COLUMN radius integer  default 3000;
ALTER TABLE atlas_app.frfs_config ADD COLUMN round_trip_ticket_limit integer NOT NULL;
ALTER TABLE atlas_app.frfs_config ADD COLUMN straight_line_distance integer  default 5000;
ALTER TABLE atlas_app.frfs_config ADD COLUMN valid_till_seconds integer  default 300;
ALTER TABLE atlas_app.frfs_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_config ADD PRIMARY KEY ( merchant_operating_city_id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_config ADD COLUMN id character varying(36) NOT NULL default md5(random()::text || clock_timestamp()::text)::uuid;
ALTER TABLE atlas_app.frfs_config ADD COLUMN route_id character varying(36) ;
ALTER TABLE atlas_app.frfs_config DROP CONSTRAINT frfs_config_pkey;
ALTER TABLE atlas_app.frfs_config ADD PRIMARY KEY ( id);