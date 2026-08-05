CREATE INDEX IF NOT EXISTS idx_transaction_created_at ON atlas_bpp_dashboard.transaction USING brin (created_at);
CREATE INDEX IF NOT EXISTS idx_transaction_endpoint_created_at ON atlas_bpp_dashboard.transaction USING btree (endpoint, created_at);
