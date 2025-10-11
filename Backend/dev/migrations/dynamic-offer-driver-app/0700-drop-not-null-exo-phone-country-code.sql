ALTER TABLE atlas_driver_offer_bpp.merchant
    ALTER COLUMN exo_phone_country_code DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant
    ALTER COLUMN exo_phone_country_code SET DEFAULT '+91';