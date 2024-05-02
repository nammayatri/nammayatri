CREATE TABLE atlas_driver_offer_bpp.driver_block_reason ();

ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN block_reason text ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN block_time_in_hours integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN reason_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_block_reason ADD PRIMARY KEY ( reason_code);