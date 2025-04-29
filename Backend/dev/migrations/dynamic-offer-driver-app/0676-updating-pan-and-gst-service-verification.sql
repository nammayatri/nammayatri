UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET pan_verification_service = 'Idfy'
WHERE merchant_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE name = 'MSIL'
);


UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET gst_verification_service = 'Idfy'
WHERE merchant_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE name = 'MSIL'
);





