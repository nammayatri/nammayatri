CREATE TABLE atlas_driver_offer_bpp.discount ();

ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN config json ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN discount_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN payment_mode text ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN plan_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN valid_from timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN valid_to timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount ADD PRIMARY KEY ( id);
