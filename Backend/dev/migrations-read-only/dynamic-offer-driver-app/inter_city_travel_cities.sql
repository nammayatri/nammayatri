CREATE TABLE atlas_driver_offer_bpp.inter_city_travel_cities ();

ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN city_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN lng double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD PRIMARY KEY ( city_name, merchant_id);

ALTER TABLE atlas_driver_offer_bpp.inter_city_travel_cities ADD COLUMN geom public.geometry(MultiPolygon);