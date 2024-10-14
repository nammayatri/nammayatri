CREATE TABLE atlas_app.time_bound_config ();

ALTER TABLE atlas_app.time_bound_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.time_bound_config ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.time_bound_config ADD COLUMN time_bound_domain text NOT NULL;
ALTER TABLE atlas_app.time_bound_config ADD COLUMN time_bounds text NOT NULL;
ALTER TABLE atlas_app.time_bound_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.time_bound_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.time_bound_config ADD PRIMARY KEY ( merchant_operating_city_id, name, time_bound_domain);