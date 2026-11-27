CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS uq_translations_global
ON atlas_driver_offer_bpp.translations (message_key, language)
WHERE merchant_operating_city_id IS NULL;

CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS uq_translations_city
ON atlas_driver_offer_bpp.translations (message_key, language, merchant_operating_city_id)
WHERE merchant_operating_city_id IS NOT NULL;
