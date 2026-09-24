UPDATE atlas_driver_offer_bpp.fare_policy AS fp
SET fare_recompute_cap_config = '{
  "caps": [
    {
      "strategy": {"tag": "PercentCap", "contents": {"percent": 10.0, "minCapAmount": null, "maxCapAmount": 100.0}},
      "appliesOn": ["ExtraKmFareComponent", "DistBasedFareComponent", "DistanceFareComponent", "ExtraDistanceFareComponent", "AmbulanceDistBasedFareComponent"]
    },
    {
      "strategy": {"tag": "PercentCap", "contents": {"percent": 10.0, "minCapAmount": null, "maxCapAmount": 100.0}},
      "appliesOn": ["RideDurationFareComponent", "TimeBasedFareComponent", "TimeFareComponent", "ExtraTimeFareComponent"]
    },
    {
      "strategy": {"tag": "FixedCap", "contents": {"amount": 20.0}},
      "appliesOn": ["WaitingCharge"]
    },
    {
      "strategy": {"tag": "FixedCap", "contents": {"amount": 50.0}},
      "appliesOn": ["TollChargesComponent"]
    }
  ]
}'
WHERE fp.id IN (
  SELECT DISTINCT fpr.fare_policy_id
  FROM atlas_driver_offer_bpp.fare_product AS fpr
  WHERE fpr.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER')
);
