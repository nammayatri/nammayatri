CREATE TABLE atlas_driver_offer_bpp.end_otp_config ();

ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN is_end_otp_required boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN trip_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN trip_mode text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD PRIMARY KEY ( merchant_operating_city_id, trip_category, trip_mode);
