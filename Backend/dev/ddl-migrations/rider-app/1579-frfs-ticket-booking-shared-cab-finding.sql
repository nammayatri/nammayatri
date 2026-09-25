-- Shared-cab FINDING bookings by route: demand strip waiting counts and the allocation tick (05 §3, §9).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_finding
  ON atlas_app.frfs_ticket_booking (route_code)
  WHERE vehicle_number IS NULL AND service_tier_type = 'SHARED_CAB' AND status = 'CONFIRMED';
