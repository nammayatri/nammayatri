-- Fleet lists in ENFORCE read across every city and several fleets at once, so the scan is bounded by
-- the fleet, not the city. Neither table had an index on the fleet (the existing ride index only
-- covers INPROGRESS rides). CONCURRENTLY so the build does not block writes; run outside a transaction.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_driver_association_fleet_owner_id
  ON atlas_driver_offer_bpp.fleet_driver_association USING btree (fleet_owner_id);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_fleet_owner_id_created_at
  ON atlas_driver_offer_bpp.ride USING btree (fleet_owner_id, created_at);
