-- Enable invalid RC deletion on replacement for MSIL_PARTNER.
UPDATE atlas_driver_offer_bpp.transporter_config
SET allow_invalid_rc_deletion_on_replacement = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_short_id = 'MSIL_PARTNER'
);
