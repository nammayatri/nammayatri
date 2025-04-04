CREATE TABLE atlas_app.multi_modal_configs ();

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN bus_filter_time_buffer_in_seconds integer NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN enable_bus_filtering boolean ;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN nearby_driver_search_radius double precision ;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_configs ADD PRIMARY KEY ( id);
