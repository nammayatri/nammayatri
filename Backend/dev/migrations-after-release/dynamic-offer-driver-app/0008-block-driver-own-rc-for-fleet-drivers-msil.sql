-- Fleet drivers on MSIL_PARTNER must use fleet-owned vehicles only.
-- Blocks own-RC onboarding/activation when the driver is linked to a fleet.
UPDATE atlas_driver_offer_bpp.transporter_config
SET block_driver_own_rc_for_fleet_drivers = true
WHERE merchant_id = (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'
);
