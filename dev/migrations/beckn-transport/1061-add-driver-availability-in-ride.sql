-- Add driver_arrival_time column in ride table
ALTER TABLE atlas_transporter.ride ADD COLUMN driver_arrival_time timestamp with time zone;

-- Update fare policy table to include waiting charges
ALTER TABLE atlas_transporter.fare_policy ADD COLUMN waiting_charge_per_min double precision;

--  Update value of waiting charges columns in fare policy
UPDATE atlas_transporter.fare_policy SET waiting_charge_per_min = 0;