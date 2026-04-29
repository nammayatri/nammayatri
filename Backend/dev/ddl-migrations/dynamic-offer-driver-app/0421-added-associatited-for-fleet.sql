

----------- changing unique  constraints for person table ------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT person_unique_mobile_number_country_code;
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT person_unique_mobile_number_country_code_role  UNIQUE (merchant_id, mobile_country_code, mobile_number_hash, role);

-------- Adding Unique constraint in driver_pan table ----------------------------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD CONSTRAINT unique_pan_card UNIQUE (pan_card_number_hash);