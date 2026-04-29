

-- Add Index for quote_id  (Production DB)
CREATE INDEX idx_frfs_quote_category_quote_id ON atlas_app.frfs_quote_category USING btree (quote_id);