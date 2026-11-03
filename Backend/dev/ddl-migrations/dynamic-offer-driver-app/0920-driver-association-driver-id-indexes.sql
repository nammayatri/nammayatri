CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_driver_association_driver_id
  ON atlas_driver_offer_bpp.fleet_driver_association (driver_id);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_operator_association_driver_id
  ON atlas_driver_offer_bpp.driver_operator_association (driver_id);
