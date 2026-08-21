CREATE INDEX IF NOT EXISTS idx_asset_release_latest
  ON atlas_app.asset_release (asset_type, merchant_id, merchant_operating_city_id, created_at DESC);
