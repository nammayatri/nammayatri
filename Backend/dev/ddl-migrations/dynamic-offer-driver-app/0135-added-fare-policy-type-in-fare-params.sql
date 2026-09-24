-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ADD column IF NOT EXISTS waiting_or_pickup_charges integer,
  ADD column IF NOT EXISTS service_charge integer,
  ADD column IF NOT EXISTS fare_policy_type character varying(255) NOT NULL DEFAULT 'NORMAL';
-- NOTE: dont need to run these queries
