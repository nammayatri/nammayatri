-- A fleet's driver list without other filters is per fleet, ordered by docs_verification_status_updated_at DESC NULLS LAST,
-- created_at DESC, with a small LIMIT; matching that order and direction lets the scan stop early with no sort.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_driver_association_onboarding_list
  ON atlas_driver_offer_bpp.fleet_driver_association USING btree (fleet_owner_id, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);
