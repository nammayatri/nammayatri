CREATE TABLE atlas_driver_offer_bpp.location ();

ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN area text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN area_code text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN building text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN city text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN country text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN door text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN full_address text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN state text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN street text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.location ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN instructions text ;
ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN extras text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.location ADD COLUMN merchant_operating_city_id character varying(36) ;