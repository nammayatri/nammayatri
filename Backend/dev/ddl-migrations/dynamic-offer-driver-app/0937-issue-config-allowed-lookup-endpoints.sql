-- Per-city whitelist for rider-app's InternalDriver chat lookups, which call
-- this app's /direct-dashboard tree with the issue lookup token. Holds
-- dashboard endpoint ids as stored in capability_endpoint, e.g.
-- 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FARE_BREAK_UP'. A request with that
-- token is rejected unless the endpoint it resolves to is listed for the
-- driver-side merchant/city in its path. NULL / empty = nothing allowed.
ALTER TABLE atlas_driver_offer_bpp.issue_config
  ADD COLUMN IF NOT EXISTS allowed_lookup_endpoints text[];
