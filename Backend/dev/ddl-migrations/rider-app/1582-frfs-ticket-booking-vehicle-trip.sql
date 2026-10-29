-- Allocation engine's FINDING scan: confirmed, unallocated shared-cab bookings per route.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_finding
  ON atlas_app.frfs_ticket_booking (route_code)
  WHERE vehicle_number IS NULL AND status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';
