UPDATE atlas_driver_offer_bpp.person SET first_name = 'UNKNOWN'
  WHERE first_name IS NULL;

ALTER TABLE atlas_driver_offer_bpp.person ALTER COLUMN first_name SET NOT NULL;