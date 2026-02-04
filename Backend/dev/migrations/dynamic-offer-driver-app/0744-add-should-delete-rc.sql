-- should_delete_rc: true for Helsinki only, false for others
UPDATE atlas_driver_offer_bpp.transporter_config
SET should_delete_rc = false;

UPDATE atlas_driver_offer_bpp.transporter_config
SET should_delete_rc = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE city = 'Helsinki' AND merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
);
