CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_entity_value_log_city_operation_created_at
  ON atlas_app.entity_value_log (merchant_operating_city_id, operation_type, created_at);
