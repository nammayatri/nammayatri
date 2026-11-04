-- vehicle_trip: a plate's ACTIVE trip, for findActiveByVehicleNumber. Hand-written because the storage YAML cannot express a partial
-- (WHERE) index. Not unique: vehicle_trip is on KV, so a unique index would only fire at drain time; one ACTIVE trip per plate is held
-- by the plate lock in code (SharedLogic.SharedCab.Trip).
CREATE INDEX IF NOT EXISTS idx_vehicle_trip_active_vehicle_number
  ON atlas_driver_offer_bpp.vehicle_trip (vehicle_number)
  WHERE status = 'ACTIVE';
