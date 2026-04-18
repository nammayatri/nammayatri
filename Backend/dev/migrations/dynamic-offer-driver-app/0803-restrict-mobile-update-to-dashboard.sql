
UPDATE atlas_driver_offer_bpp.transporter_config
SET restrict_mobile_update_to_dashboard = false
WHERE merchant_operating_city_id IN (
  SELECT id
  FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE city = 'Helsinki'
);