CREATE TABLE atlas_driver_offer_bpp.offer ();

ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN discount_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN discount_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN max_discount double precision ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN offer_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN offer_eligibility_json_logic json ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN offer_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN sponsored_by text ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN title text ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN tnc text ;
ALTER TABLE atlas_driver_offer_bpp.offer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.offer ADD PRIMARY KEY ( id);

