ALTER TABLE  atlas_driver_offer_bpp.merchant ADD COLUMN aadhaar_verification_req boolean NOT NULL DEFAULT false;
ALTER TABLE  atlas_driver_offer_bpp.driver_information ADD COLUMN aadhaar_verified boolean NOT NULL DEFAULT false;
