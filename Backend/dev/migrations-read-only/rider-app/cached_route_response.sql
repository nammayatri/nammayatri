CREATE TABLE atlas_app.cached_route_response ();

ALTER TABLE atlas_app.cached_route_response ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN distance integer ;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN drop_geohash text NOT NULL;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN duration integer ;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN hour_of_day integer NOT NULL;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN pickup_geohash text NOT NULL;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN routes json ;
ALTER TABLE atlas_app.cached_route_response ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cached_route_response ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.cached_route_response ADD COLUMN avoid_toll boolean ;