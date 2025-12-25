UPDATE atlas_driver_offer_bpp.merchant SET country = 'India' where city = 'Bangalore' or city = 'Kochi' or city = 'Kolkata';
UPDATE atlas_driver_offer_bpp.merchant SET country = 'France' where city = 'Paris';
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN country SET NOT NULL;