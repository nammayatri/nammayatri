UPDATE atlas_driver_offer_bpp.transporter_config
SET allow_dl_reupload = true, allow_pan_reupload = true, allow_aadhaar_reupload = true, allow_gst_reupload = true
WHERE merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
    AND city = 'Delhi'
);