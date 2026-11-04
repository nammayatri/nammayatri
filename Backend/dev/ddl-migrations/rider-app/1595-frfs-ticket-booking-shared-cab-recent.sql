-- A shared-cab booking stays CONFIRMED after its ride, so the live scans read only recently written rows.
-- The allocation tick, the expiry watchdog and the degraded sweep: a city's CONFIRMED bookings by updated_at.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_city_updated
  ON atlas_app.frfs_ticket_booking (merchant_operating_city_id, updated_at)
  WHERE status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';
-- A plate's recent bookings (driver session, riders on board, unboarded closes). No status predicate: the driver view reads every
-- non-terminal status, which a CONFIRMED-only partial index could not serve.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_vehicle_updated
  ON atlas_app.frfs_ticket_booking (vehicle_number, updated_at)
  WHERE vehicle_number IS NOT NULL AND service_tier_type = 'SHARED_CAB';
