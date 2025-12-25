CREATE TABLE atlas_driver_offer_bpp.station ();

ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN address text ;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN possible_types text ;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN time_bounds text  default 'Unbounded';
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.station ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.station ADD PRIMARY KEY ( id);
