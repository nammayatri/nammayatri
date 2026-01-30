-- Set HATCHBACK for all cities except Helsinki (Helsinki = no fallback, enforce DVC validation)
UPDATE atlas_driver_offer_bpp.transporter_config
SET missing_mapping_fallback_variant = 'HATCHBACK'
WHERE merchant_operating_city_id NOT IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Helsinki' and merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
);
