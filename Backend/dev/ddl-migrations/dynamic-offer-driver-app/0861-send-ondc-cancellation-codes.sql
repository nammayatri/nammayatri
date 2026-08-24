ALTER TABLE atlas_driver_offer_bpp.beckn_config
  ADD COLUMN IF NOT EXISTS send_ondc_cancellation_codes boolean;
