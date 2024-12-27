CREATE TABLE atlas_driver_offer_bpp.route ();

ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN color text ;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN end_lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN end_lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN long_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN polyline text ;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN short_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN start_lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN start_lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.route ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.route ADD COLUMN round_route_code text ;