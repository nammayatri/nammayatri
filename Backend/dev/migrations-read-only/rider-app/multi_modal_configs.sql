CREATE TABLE atlas_app.multi_modal_configs ();

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN bus_filter_time_buffer_in_seconds integer NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN enable_bus_filtering boolean ;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN nearby_driver_search_radius double precision ;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_configs ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN suburban_booking_allowed boolean ;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN straight_line_threshold integer  default 300;


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN metro_booking_allowed boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN maximum_walk_distance integer  default 600;


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN make_multi_modal_search boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN permissible_modes text []  default '{Walk, Bus, MetroRail, Subway}';
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN multimodal_testing boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN minimum_walk_distance integer  default 100;
ALTER TABLE atlas_app.multi_modal_configs ADD COLUMN max_allowed_public_transport_legs integer  default 2;