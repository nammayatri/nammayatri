UPDATE atlas_driver_offer_bpp.transporter_config
SET allow_disable_fleet_on_rejection_doc = true
WHERE merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE city = 'Helsinki'
);