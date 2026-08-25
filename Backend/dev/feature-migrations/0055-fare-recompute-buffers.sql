-- fare_recompute_cap_config is required alongside fare_recompute_cap_enabled
-- for the cap to actually apply. Value below is just an example of the
-- expected shape (10% PercentCap on RideFare) -- replace with MSIL's actual config.
UPDATE atlas_driver_offer_bpp.fare_policy AS fp
SET fare_recompute_cap_enabled = true,
    fare_recompute_cap_config = '{"caps":[{"strategy":{"tag":"PercentCap","contents":{"percent":10.0,"minCapAmount":null,"maxCapAmount":null}},"appliesOn":["RideFare"]}]}'
WHERE fp.id IN (
  SELECT DISTINCT fpr.fare_policy_id
  FROM atlas_driver_offer_bpp.fare_product AS fpr
  WHERE fpr.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
);
