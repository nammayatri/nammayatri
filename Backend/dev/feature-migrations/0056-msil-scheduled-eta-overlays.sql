-- SCHEDULED_AT_RISK driver overlay for the ETA-feasibility monitor. ETA warns with a single flat key
-- (no cancellation-cost situation variants — that logic is distance-mode only), so one row per city/language.
-- Idempotent (NOT EXISTS); md5 id is deterministic per (city, key). A non-null title also makes
-- sendOverlay surface a push when the app is closed (MSIL: in-app if open, push if closed).

INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, title, description, ok_button_text)
SELECT
  md5(moc.id || '-SCHEDULED_AT_RISK'),
  'ENGLISH',
  moc.merchant_id,
  moc.id,
  'SCHEDULED_AT_RISK',
  'You have an upcoming ride',
  'Please reach pick up point on time to avoid risk of cancellation.',
  'Okay'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
    WHERE mo.merchant_operating_city_id = moc.id
      AND mo.overlay_key = 'SCHEDULED_AT_RISK'
      AND mo.language = 'ENGLISH'
  );
