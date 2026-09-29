-- One counter row per (subject, city): the atomic upsert bump (Storage.Queries.SharedCabBlameCountExtra.bump)
-- conflicts on this key. A unique index is all ON CONFLICT (cols) needs, and IF NOT EXISTS keeps re-runs
-- safe (same shape as 0830-vendor-split-details-daily-plan-unique-index.sql).
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS shared_cab_blame_count_subject_city_key
  ON atlas_app.shared_cab_blame_count (subject_type, subject_id, merchant_operating_city_id);
