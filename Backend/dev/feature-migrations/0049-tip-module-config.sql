DO $$
DECLARE
  v_merchant_id TEXT;
BEGIN
  SELECT m.id INTO v_merchant_id FROM atlas_app.merchant m WHERE m.short_id = 'NAMMA_YATRI' LIMIT 1;
  IF v_merchant_id IS NULL THEN
    RAISE NOTICE 'NAMMA_YATRI merchant not found, skipping tip_module_config default';
    RETURN;
  END IF;

  UPDATE atlas_app.rider_config rc
  SET tip_module_config = '{"showAfterSec":45,"repeatIntervalSec":60,"maxPrompts":1}'::json,
      updated_at = now()
  FROM atlas_app.merchant_operating_city moc
  WHERE rc.merchant_operating_city_id = moc.id
    AND moc.merchant_id = v_merchant_id
    AND rc.tip_module_config IS NULL;

  RAISE NOTICE 'tip_module_config default set for NAMMA_YATRI cities';
END $$;
