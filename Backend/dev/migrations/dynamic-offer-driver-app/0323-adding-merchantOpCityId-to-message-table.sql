ALTER TABLE atlas_driver_offer_bpp.message
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);