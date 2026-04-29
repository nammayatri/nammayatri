ALTER TABLE atlas_app.translations
  ALTER COLUMN merchant_operating_city_id DROP NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_translations_global
ON atlas_app.translations (message_key, language)
WHERE merchant_operating_city_id IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS uq_translations_city
ON atlas_app.translations (message_key, language, merchant_operating_city_id)
WHERE merchant_operating_city_id IS NOT NULL;