-- Update fare policy table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN waiting_charge_per_min double precision;

-- Update value of waiting charges columns in fare policy
UPDATE atlas_driver_offer_bpp.fare_policy SET waiting_charge_per_min = 1
WHERE merchant_id IN ('favorit0-0000-0000-0000-00000favorit', 'nearest-drivers-testing-organization');

-- Update fare params table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN waiting_charge_per_min double precision;
