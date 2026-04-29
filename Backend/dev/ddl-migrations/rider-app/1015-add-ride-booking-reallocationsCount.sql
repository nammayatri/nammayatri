ALTER TABLE atlas_app.booking
  ADD COLUMN reallocations_count integer DEFAULT 0;

ALTER TABLE atlas_app.booking RENAME COLUMN rider_transaction_id TO "transaction_id";
ALTER TABLE atlas_app.booking RENAME COLUMN bpp_booking_id TO "bpp_ride_booking_id";