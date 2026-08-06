ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN ondc_cancellation_reason_id text;

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN created_at timestamp with time zone;

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason
  ADD COLUMN updated_at timestamp with time zone;
