CREATE TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ();

ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN request_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN request_message text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN status_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN transaction_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_otp_verify ADD PRIMARY KEY ( id);