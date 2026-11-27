-- Per-city whitelist for in-house chat lookups (ApiCall flow steps).
-- Holds dashboard endpoint ids as stored in capability_endpoint, e.g.
-- 'RIDER_MANAGEMENT/RIDE/GET_RIDE_INFO'. A lookup call made with the issue
-- lookup token is rejected unless the endpoint the request resolves to is
-- listed for the city in its path. NULL / empty = no lookups allowed.
ALTER TABLE atlas_app.issue_config
  ADD COLUMN IF NOT EXISTS allowed_lookup_endpoints text[];

-- External lookups are no longer executed; deactivate any that exist so the
-- dashboard shows them as inactive.
UPDATE atlas_app.issue_api_integration
  SET is_active = false, updated_at = now()
  WHERE kind = 'External';
