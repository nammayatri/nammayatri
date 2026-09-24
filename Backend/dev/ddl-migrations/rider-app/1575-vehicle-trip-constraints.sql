-- At most one live (ACTIVE/PAUSED) trip per vehicle; also serves findActiveByVehicleNumber.
CREATE UNIQUE INDEX IF NOT EXISTS idx_vehicle_trip_live_vehicle_number
  ON atlas_app.vehicle_trip (vehicle_number)
  WHERE status IN ('ACTIVE', 'PAUSED');

ALTER TABLE atlas_app.vehicle_trip
  ADD CONSTRAINT vehicle_trip_ended_at_iff_closed
  CHECK ((ended_at IS NULL) = (status IN ('ACTIVE', 'PAUSED')));

ALTER TABLE atlas_app.vehicle_trip
  ADD CONSTRAINT vehicle_trip_offline_boardings_non_negative
  CHECK (offline_boardings >= 0);
