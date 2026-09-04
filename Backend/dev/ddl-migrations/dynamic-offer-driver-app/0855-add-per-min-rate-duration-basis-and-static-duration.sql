-- Basis for perMinRateSections pricing: 'TotalDuration' (default when NULL) or 'TrafficDelayDuration'
-- (search_request/booking estimated_static_duration columns come from the generated read-only migrations)
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS per_min_rate_duration_basis character varying(255);
