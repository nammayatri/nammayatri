-- The GST/PAN linkage check is opt-in (MSIL_PARTNER only). The is_gst_pan_link_check_required
-- column defaults to false, so all other merchants skip the check.
-- Enable it explicitly for MSIL here (merchant level).
UPDATE atlas_driver_offer_bpp.transporter_config
SET is_gst_pan_link_check_required = true
WHERE merchant_id = (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'
);
