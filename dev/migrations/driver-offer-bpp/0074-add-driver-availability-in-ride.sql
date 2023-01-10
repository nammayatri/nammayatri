-- Add driver_arrival_time column in ride table
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_arrival_time timestamp with time zone;

-- Update fare policy table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN waiting_charge_per_min double precision;

-- Update value of waiting charges columns in fare policy
UPDATE atlas_driver_offer_bpp.fare_policy SET waiting_charge_per_min = 1
WHERE merchant_id IN ('favorit0-0000-0000-0000-00000favorit', 'nearest-drivers-testing-organization');

-- Update fare params table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN waiting_charge_per_min double precision;

-- Add waiting_threshold column in transport_config
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN waiting_time_estimated_threshold int;

-- Update value of waiting_threshold column in transport_config
UPDATE atlas_driver_offer_bpp.transporter_config SET waiting_time_estimated_threshold = 3;
