-- Shared-cab partial indexes. Kept as hand-written DDL because the storage YAML cannot express a partial (WHERE) index; every
-- shared-cab column comes from the YAML specs (migrations-read-only).

-- vehicle_trip: a plate's live (ACTIVE/PAUSED) trip, for findActiveByVehicleNumber. Not unique: vehicle_trip is on KV, so a unique
-- index or a CHECK would only fire at drain time; one live trip per plate is held by the plate lock in code (SharedCab.Session).
CREATE INDEX IF NOT EXISTS idx_vehicle_trip_live_vehicle_number
  ON atlas_app.vehicle_trip (vehicle_number)
  WHERE status IN ('ACTIVE', 'PAUSED');

-- frfs_ticket_booking: the allocation engine's FINDING scan, confirmed unallocated shared-cab bookings per route.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_finding
  ON atlas_app.frfs_ticket_booking (route_code)
  WHERE vehicle_number IS NULL AND status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';

-- frfs_ticket_booking: a shared cab's live bookings by plate, for the route change / end guard and session recovery.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_vehicle
  ON atlas_app.frfs_ticket_booking (vehicle_number)
  WHERE vehicle_number IS NOT NULL AND service_tier_type = 'SHARED_CAB';

-- frfs_ticket_booking: the allocation tick's per-city scan of live shared-cab bookings, FINDING and ALLOCATED.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_city
  ON atlas_app.frfs_ticket_booking (merchant_operating_city_id)
  WHERE status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';

-- frfs_ticket_booking: a shared-cab booking stays CONFIRMED after its ride, so the live scans read only recently written rows.
-- The allocation tick, the expiry watchdog and the degraded sweep: a city's CONFIRMED bookings by updated_at.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_city_updated
  ON atlas_app.frfs_ticket_booking (merchant_operating_city_id, updated_at)
  WHERE status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';

-- frfs_ticket_booking: a plate's recent bookings (driver session, riders on board, unboarded closes). No status predicate: the
-- driver view reads every non-terminal status, which a CONFIRMED-only partial index could not serve.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_vehicle_updated
  ON atlas_app.frfs_ticket_booking (vehicle_number, updated_at)
  WHERE vehicle_number IS NOT NULL AND service_tier_type = 'SHARED_CAB';
