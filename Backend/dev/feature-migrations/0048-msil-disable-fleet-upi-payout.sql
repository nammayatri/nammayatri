UPDATE atlas_driver_offer_bpp.transporter_config tc
SET    fleet_upi_payout_enabled = false
FROM   atlas_driver_offer_bpp.merchant_operating_city moc
JOIN   atlas_driver_offer_bpp.merchant m
         ON m.id = moc.merchant_id
WHERE  tc.merchant_operating_city_id = moc.id
  AND  m.short_id = 'MSIL_PARTNER';

