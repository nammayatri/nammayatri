ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT person_unique_mobile_number_country_code;
ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT unique_email;
ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT unique_identifier;
DROP INDEX idx_person_mobile_num;

ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT person_unique_mobile_number_country_code UNIQUE (merchant_id, mobile_country_code, mobile_number_hash);
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_email UNIQUE (merchant_id, email);
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_identifier UNIQUE (merchant_id, identifier);
CREATE INDEX idx_person_mobile_num ON atlas_driver_offer_bpp.person USING btree (merchant_id, mobile_country_code, mobile_number_hash);

-- RIDER DETAILS
ALTER TABLE atlas_driver_offer_bpp.rider_details drop CONSTRAINT ride_details_unique_mobile_number;
DROP INDEX idx_rider_details_mobile_number;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD CONSTRAINT ride_details_unique_mobile_number UNIQUE (merchant_id, mobile_number_hash, mobile_country_code);
CREATE INDEX idx_rider_details_mobile_number ON atlas_driver_offer_bpp.rider_details USING btree (merchant_id, mobile_number_hash);
