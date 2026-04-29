CREATE INDEX idx_aadhaar_verification_aadhaar_number_hash ON atlas_app.aadhaar_verification  USING btree (aadhaar_number_hash);
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN aadhaar_verification_service SET NOT NULL;
ALTER TABLE atlas_app.merchant ALTER COLUMN aadhaar_verification_try_limit SET NOT NULL;
