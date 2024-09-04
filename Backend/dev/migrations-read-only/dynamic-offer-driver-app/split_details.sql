CREATE TABLE atlas_driver_offer_bpp.split_details ();

ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN amount_percentage double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN fixed_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.split_details ADD PRIMARY KEY ( id);