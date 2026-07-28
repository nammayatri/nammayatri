-- RUN IN MASTER, UAT ONLY
UPDATE atlas_driver_offer_bpp.transporter_config
SET deactivate_rc_on_unlink = false
WHERE merchant_id = (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'
);
