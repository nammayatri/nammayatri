CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_ride_fleet_owner_id_inprogress
  ON atlas_driver_offer_bpp.ride USING btree (fleet_owner_id)
  WHERE status = 'INPROGRESS';
