ALTER TABLE atlas_driver_offer_bpp.transporter_config
  ADD COLUMN IF NOT EXISTS prefer_ondc_cancellation_reason_id boolean;
