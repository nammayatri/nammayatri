ALTER TABLE atlas_app.rider_config ADD COLUMN IF NOT EXISTS tip_module_config json;
ALTER TABLE atlas_app.estimate ADD COLUMN IF NOT EXISTS tip_module_config json;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN IF NOT EXISTS tip_module_config json;

UPDATE atlas_app.rider_config rc
SET tip_module_config = '{"showAfterSec":45,"repeatIntervalSec":60,"maxPrompts":1}'::json,
    updated_at = now()
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
WHERE rc.merchant_operating_city_id = moc.id
  AND m.short_id = 'NAMMA_YATRI'
  AND rc.tip_module_config IS NULL;

UPDATE atlas_driver_offer_bpp.transporter_config tc
SET is_dynamic_pricing_qar_cal_enabled = true,
    updated_at = now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
JOIN atlas_driver_offer_bpp.merchant m ON m.id = moc.merchant_id
WHERE tc.merchant_operating_city_id = moc.id
  AND m.short_id = 'NAMMA_YATRI_PARTNER'
  AND (tc.is_dynamic_pricing_qar_cal_enabled IS DISTINCT FROM true);
