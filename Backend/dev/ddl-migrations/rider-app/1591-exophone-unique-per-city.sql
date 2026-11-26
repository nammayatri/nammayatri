ALTER TABLE atlas_app.exophone DROP CONSTRAINT IF EXISTS exophone_unique_primary_phone;
ALTER TABLE atlas_app.exophone DROP CONSTRAINT IF EXISTS exophone_unique_backup_phone;
DROP INDEX IF EXISTS atlas_app.exophone_unique_primary_phone;
DROP INDEX IF EXISTS atlas_app.exophone_unique_backup_phone;

CREATE UNIQUE INDEX IF NOT EXISTS exophone_unique_primary_phone_per_city
  ON atlas_app.exophone (merchant_operating_city_id, primary_phone);

CREATE UNIQUE INDEX IF NOT EXISTS exophone_unique_backup_phone_per_city
  ON atlas_app.exophone (merchant_operating_city_id, backup_phone)
  WHERE backup_phone IS NOT NULL;
