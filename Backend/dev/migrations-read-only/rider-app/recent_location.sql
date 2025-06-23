CREATE TABLE atlas_app.recent_location ();

ALTER TABLE atlas_app.recent_location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_lat_long text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_route_code text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_stop_code text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN frequency integer NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN route_id text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recent_location ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_lon double precision ;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_lat double precision ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_stop_lon double precision ;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_data_stop_lat double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN stop_lon double precision ;
ALTER TABLE atlas_app.recent_location ADD COLUMN stop_lat double precision ;
ALTER TABLE atlas_app.recent_location ADD COLUMN stop_code text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN route_code text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN address text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN from_stop_name text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN from_stop_code text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN fare double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.recent_location ADD COLUMN to_geohash text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN from_geohash text ;
