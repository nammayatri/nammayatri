-- The individual-PAN check is opt-in (MSIL_PARTNER only). The individual_pan_check
-- column is added with no default, so all merchants start NULL (no check / clean skip).
-- Enable it explicitly for MSIL here (merchant level).
UPDATE atlas_driver_offer_bpp.transporter_config
SET individual_pan_check = true
WHERE merchant_id = (
  SELECT id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER'
);
