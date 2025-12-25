CREATE TABLE atlas_driver_offer_bpp.fare_product ();

ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN area text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN merchant_operating_city_id character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN trip_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN vehicle_variant character varying (60) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_product ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN search_source text NOT NULL default 'ALL';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fare_product ADD COLUMN disable_recompute boolean ;