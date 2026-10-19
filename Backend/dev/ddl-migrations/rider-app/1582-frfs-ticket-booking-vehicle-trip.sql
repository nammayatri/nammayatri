-- NOT VALID then VALIDATE: the table scan runs under the weaker SHARE UPDATE EXCLUSIVE lock.
ALTER TABLE atlas_app.frfs_ticket_booking
  ADD CONSTRAINT frfs_ticket_booking_vehicle_trip_id_fkey
  FOREIGN KEY (vehicle_trip_id) REFERENCES atlas_app.vehicle_trip (id) NOT VALID;
ALTER TABLE atlas_app.frfs_ticket_booking VALIDATE CONSTRAINT frfs_ticket_booking_vehicle_trip_id_fkey;

-- Allocation engine's FINDING scan: confirmed, unallocated shared-cab bookings per route.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_frfs_ticket_booking_shared_cab_finding
  ON atlas_app.frfs_ticket_booking (route_code)
  WHERE vehicle_number IS NULL AND status = 'CONFIRMED' AND service_tier_type = 'SHARED_CAB';
