CREATE INDEX idx_aadhaar_card_merchant_dob ON atlas_driver_offer_bpp.aadhaar_card USING btree (merchant_id, date_of_birth);
