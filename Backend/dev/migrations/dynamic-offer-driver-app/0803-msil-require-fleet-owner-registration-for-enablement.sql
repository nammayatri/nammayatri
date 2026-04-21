UPDATE atlas_driver_offer_bpp.transporter_config
SET require_fleet_owner_registration_for_enablement = true,
allow_pan_aadhaar_linkage = true
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);
