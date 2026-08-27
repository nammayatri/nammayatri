-- SCHEDULED_AT_RISK driver overlays for the ETA-feasibility monitor. The job appends the
-- cancellation-cost situation suffix, so all 3 variants must exist per language/city.
-- Idempotent (NOT EXISTS); md5 id is deterministic per (city, key). A non-null title also makes
-- sendOverlay surface a push when the app is closed (MSIL: in-app if open, push if closed).

INSERT INTO atlas_driver_offer_bpp.merchant_overlay
  (id, language, merchant_id, merchant_operating_city_id, overlay_key, title, description, ok_button_text)
SELECT
  md5(moc.id || '-' || key),
  'ENGLISH',
  moc.merchant_id,
  moc.id,
  key,
  'You have an upcoming ride',
  'Please reach pick up point on time to avoid risk of cancellation.',
  'Okay'
FROM atlas_driver_offer_bpp.merchant_operating_city moc
CROSS JOIN unnest(ARRAY[
  'SCHEDULED_AT_RISK_NON_CANCELLABLE',
  'SCHEDULED_AT_RISK_FEE_APPLIES',
  'SCHEDULED_AT_RISK_FREE_CANCEL'
]) AS key
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_driver_offer_bpp.merchant_overlay mo
    WHERE mo.merchant_operating_city_id = moc.id
      AND mo.overlay_key = key
      AND mo.language = 'ENGLISH'
  );
