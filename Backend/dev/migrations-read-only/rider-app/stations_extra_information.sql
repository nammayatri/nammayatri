CREATE TABLE atlas_app.stations_extra_information ();

ALTER TABLE atlas_app.stations_extra_information ADD COLUMN address text ;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN station_id text NOT NULL;
ALTER TABLE atlas_app.stations_extra_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.stations_extra_information ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.stations_extra_information ADD COLUMN suggested_destinations text ;