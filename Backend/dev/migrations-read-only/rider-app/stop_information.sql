CREATE TABLE atlas_app.stop_information ();

ALTER TABLE atlas_app.stop_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stop_information ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stop_information ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stop_information ADD COLUMN stop_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stop_information ADD COLUMN stop_order integer NOT NULL;
ALTER TABLE atlas_app.stop_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stop_information ADD COLUMN waiting_time_end timestamp with time zone ;
ALTER TABLE atlas_app.stop_information ADD COLUMN waiting_time_start timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.stop_information ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.stop_information ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.stop_information ADD PRIMARY KEY ( id);