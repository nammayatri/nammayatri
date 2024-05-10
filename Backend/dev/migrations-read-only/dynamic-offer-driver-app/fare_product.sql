CREATE TABLE atlas_driver_offer_bpp.fare_product ();

ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN area text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN time_bounds text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN trip_category text NOT NULL;