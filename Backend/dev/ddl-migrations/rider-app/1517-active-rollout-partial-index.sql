-- Partial index for fetching only active rollouts (base version or running experiments)
-- Used by findActiveByMerchantOpCityAndDomain in ConfigPilot
CREATE INDEX IF NOT EXISTS idx_app_dynamic_logic_rollout_active
  ON atlas_app.app_dynamic_logic_rollout (merchant_operating_city_id, domain)
  WHERE (is_base_version = true OR experiment_status = 'RUNNING');
