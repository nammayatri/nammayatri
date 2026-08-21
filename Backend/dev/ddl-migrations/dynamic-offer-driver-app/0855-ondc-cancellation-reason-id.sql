ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN IF NOT EXISTS ondc_cancellation_reason_id text;

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN IF NOT EXISTS created_at timestamp with time zone;

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN IF NOT EXISTS updated_at timestamp with time zone;
