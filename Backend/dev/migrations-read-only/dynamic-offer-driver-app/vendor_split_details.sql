CREATE TABLE atlas_driver_offer_bpp.vendor_split_details ();

ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN area text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN split_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN split_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vendor_split_details ADD PRIMARY KEY ( area, merchant_operating_city_id, vehicle_variant, vendor_id);
