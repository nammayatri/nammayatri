CREATE TABLE atlas_driver_offer_bpp.payout_split_config ();

ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN area text ;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN bank_details text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_split_config ADD PRIMARY KEY ( id);
