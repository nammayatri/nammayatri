-- A plate's live (ACTIVE/PAUSED) trip, for findActiveByVehicleNumber. Not unique: vehicle_trip is on KV, so a unique index or a CHECK
-- would only fire at drain time; one live trip per plate is held by the plate lock in code (SharedCab.Session).
CREATE INDEX IF NOT EXISTS idx_vehicle_trip_live_vehicle_number
  ON atlas_app.vehicle_trip (vehicle_number)
  WHERE status IN ('ACTIVE', 'PAUSED');
