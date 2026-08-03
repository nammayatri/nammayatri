UPDATE atlas_driver_offer_bpp.transporter_config AS tc
SET driver_wallet_config =
  (
    jsonb_set(
      jsonb_set(
        tc.driver_wallet_config::jsonb,
        '{fareRecomputeBufferPercent}',
        '10'::jsonb
      ),
      '{fareRecomputeBufferAmount}',
      '100'::jsonb
    )
  )::json
FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
WHERE tc.merchant_operating_city_id = moc.id
  AND tc.driver_wallet_config IS NOT NULL
  AND moc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER');

UPDATE atlas_driver_offer_bpp.fare_policy AS fp
SET fare_recompute_cap_enabled = true
WHERE fp.id IN (
  SELECT DISTINCT fpr.fare_policy_id
  FROM atlas_driver_offer_bpp.fare_product AS fpr
  WHERE fpr.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
);
