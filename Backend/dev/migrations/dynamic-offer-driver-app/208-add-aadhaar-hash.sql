ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN aadhaar_number_hash Text;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN is_verified Boolean default true;
ALTER TABLE atlas_driver_offer_bpp.aadhaar_verification ADD COLUMN updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;
CREATE INDEX idx_aadhaar_verification_aadhaar_number_hash ON atlas_driver_offer_bpp.aadhaar_verification  USING btree (aadhaar_number_hash);