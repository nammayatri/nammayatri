-- Replace the global single-column unique on exophone phone numbers with a per-city unique.
-- Non-CONCURRENTLY: this file also drops a constraint (ALTER TABLE), so the migration runner
-- executes it inside a transaction; CONCURRENTLY is illegal there (SqlError 25001). exophone is
-- a small config table, so a plain (locking) index build/drop is negligible.
ALTER TABLE atlas_driver_offer_bpp.exophone DROP CONSTRAINT IF EXISTS exophone_unique_primary_phone;
ALTER TABLE atlas_driver_offer_bpp.exophone DROP CONSTRAINT IF EXISTS exophone_unique_backup_phone;
DROP INDEX IF EXISTS atlas_driver_offer_bpp.exophone_unique_primary_phone;
DROP INDEX IF EXISTS atlas_driver_offer_bpp.exophone_unique_backup_phone;

CREATE UNIQUE INDEX IF NOT EXISTS exophone_unique_primary_phone_per_city
  ON atlas_driver_offer_bpp.exophone (merchant_operating_city_id, primary_phone);

CREATE UNIQUE INDEX IF NOT EXISTS exophone_unique_backup_phone_per_city
  ON atlas_driver_offer_bpp.exophone (merchant_operating_city_id, backup_phone)
  WHERE backup_phone IS NOT NULL;
