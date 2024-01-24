CREATE TABLE atlas_app.rider_config ();

ALTER TABLE atlas_app.rider_config ADD COLUMN app_url text NOT NULL default 'nammayatri.in/link/rider/rmxw';
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_emergency_contact_added_message boolean NOT NULL default true;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_local_police_support boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_support_for_safety boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN local_police_number text ;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_end_time integer NOT NULL default 21600;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_start_time integer NOT NULL default 75600;
ALTER TABLE atlas_app.rider_config ADD COLUMN time_diff_from_utc integer NOT NULL default 19800;
ALTER TABLE atlas_app.rider_config ADD COLUMN tracking_short_url_pattern text NOT NULL default 'nammayatri.in/t/';
ALTER TABLE atlas_app.rider_config ADD COLUMN video_file_size_upper_limit integer NOT NULL default 15000000;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.rider_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD PRIMARY KEY ( merchant_operating_city_id);