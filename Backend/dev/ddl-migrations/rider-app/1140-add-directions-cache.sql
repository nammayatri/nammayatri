CREATE TABLE IF NOT EXISTS  atlas_app.directions_cache(
  id character(36) NOT NULL PRIMARY KEY,
  origin_hash TEXT NOT NULL,
  dest_hash TEXT NOT NULL,
  slot INTEGER NOT  NULL,
  response TEXT NOT NULL,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
  CONSTRAINT unique_optimal_path UNIQUE (origin_hash, dest_hash, slot)
);
CREATE INDEX idx_directions_caching ON atlas_app.directions_cache USING btree (origin_hash, dest_hash, slot);