-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN IF NOT EXISTS return_fee JSON,
ADD COLUMN IF NOT EXISTS booth_charges JSON,
ADD COLUMN IF NOT EXISTS per_luggage_charge double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS booth_charge double precision,
ADD COLUMN IF NOT EXISTS luggage_charge double precision,
ADD COLUMN IF NOT EXISTS return_fee_charge double precision;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
