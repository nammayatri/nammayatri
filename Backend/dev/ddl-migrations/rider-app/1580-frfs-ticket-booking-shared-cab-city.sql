-- The allocation tick's per-city scan of live shared-cab bookings, FINDING and ALLOCATED (05 §3).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_city
  ON atlas_app.frfs_ticket_booking (merchant_operating_city_id)
  WHERE status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';
