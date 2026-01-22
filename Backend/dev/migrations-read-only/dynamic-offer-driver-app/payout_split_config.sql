CREATE TABLE atlas_driver_offer_bpp.payout_split_config ();

ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN area text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN bank_details text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN vendor_split_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD PRIMARY KEY ( area, vehicle_variant, vendor_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_split_config DROP CONSTRAINT payout_split_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD PRIMARY KEY ( area, merchant_operating_city_id, vehicle_variant, vendor_id);