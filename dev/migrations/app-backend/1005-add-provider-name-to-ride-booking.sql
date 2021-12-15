ALTER TABLE atlas_app.ride_booking ADD COLUMN provider_name VARCHAR(255);

UPDATE atlas_app.ride_booking
  SET provider_name = quote.provider_name
  FROM (SELECT id, provider_name FROM atlas_app.quote) as quote
  WHERE ride_booking.quote_id = quote.id;

ALTER TABLE atlas_app.ride_booking ALTER COLUMN provider_name SET NOT NULL;