CREATE TABLE atlas_app.journey_route_details ();

ALTER TABLE atlas_app.journey_route_details ADD COLUMN frequency integer ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN from_station_id character varying(36) ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN line_color text ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN line_color_code text ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN platform_number text ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN route_long_name text ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN to_station_id character varying(36) ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_route_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_route_details ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.journey_route_details ADD COLUMN sub_leg_order integer ;


------- SQL updates -------

ALTER TABLE atlas_app.journey_route_details ADD COLUMN route_id character varying(36) ;