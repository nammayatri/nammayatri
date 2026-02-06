-- Add driver cancellation deduction column to fare_parameters table
-- This stores the amount deducted from the driver's earnings on the current ride
-- due to a cancellation penalty from a previous ride
ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS driver_cancellation_deduction_on_previous_ride double precision;
