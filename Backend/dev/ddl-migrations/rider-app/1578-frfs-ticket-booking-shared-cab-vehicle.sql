-- A shared cab's live bookings by plate: route change / end guard and session recovery (04 §3, §4).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_vehicle
  ON atlas_app.frfs_ticket_booking (vehicle_number)
  WHERE vehicle_number IS NOT NULL AND service_tier_type = 'SHARED_CAB';
