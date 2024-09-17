CREATE TABLE atlas_app.multi_modal_time_frame ();

ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_time_frame ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_time_frame ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.multi_modal_time_frame ALTER COLUMN start_time TYPE time without time zone;
ALTER TABLE atlas_app.multi_modal_time_frame ALTER COLUMN end_time TYPE time without time zone;