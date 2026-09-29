-- One counter row per (subject, city): the atomic upsert bump (Storage.Queries.SharedCabBlameCountExtra.bump)
-- conflicts on this key. CONCURRENTLY then attach: no exclusive table lock.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS shared_cab_blame_count_subject_city_key
  ON atlas_app.shared_cab_blame_count (subject_type, subject_id, merchant_operating_city_id);

ALTER TABLE atlas_app.shared_cab_blame_count
  ADD CONSTRAINT shared_cab_blame_count_subject_city_key
  UNIQUE USING INDEX shared_cab_blame_count_subject_city_key;
