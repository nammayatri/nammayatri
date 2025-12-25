CREATE TABLE atlas_driver_offer_bpp.merchant_operating_city ();

ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN city character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN country text NOT NULL default 'India';
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN language text NOT NULL default 'ENGLISH';
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN lat double precision NOT NULL default 12.971599;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN lon double precision NOT NULL default 77.594566;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN merchant_short_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN support_number text ;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN distance_unit character varying(255) ;
