ALTER TABLE atlas_transporter.one_way_quote ADD COLUMN estimated_finish_time timestamp with time zone;

WITH QuoteStartTime AS (
  SELECT T2.id, T3.start_time
  FROM atlas_transporter.one_way_quote AS T1
  JOIN atlas_transporter.quote AS T2
  ON T2.id = T1.quote_id
  JOIN atlas_transporter.search_request AS T3
  ON T3.id = T2.request_id
)
UPDATE atlas_transporter.one_way_quote AS T1
  SET estimated_finish_time = (SELECT T2.start_time FROM QuoteStartTime AS T2 WHERE T1.quote_id = T2.id);
-- Technically it ^^^ is not correct, but previously start_time had been used to count totalFare,
-- but now estimated_finish_time used instead, so i made estimated_finish_time = start_time for old rides.
ALTER TABLE atlas_transporter.one_way_quote ALTER COLUMN estimated_finish_time SET NOT NULL;

ALTER TABLE atlas_transporter.booking ADD COLUMN fare_product_type varchar(255);
UPDATE atlas_transporter.booking AS T1 SET fare_product_type = 'RENTAL'
  WHERE T1.to_location_id IS NULL;
UPDATE atlas_transporter.booking AS T1 SET fare_product_type = 'ONE_WAY'
  WHERE T1.fare_product_type IS NULL;
ALTER TABLE atlas_transporter.booking ALTER COLUMN fare_product_type SET NOT NULL;

CREATE TABLE atlas_transporter.one_way_booking (
    booking_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_transporter.booking (id),
    to_location_id character(36) NOT NULL REFERENCES atlas_transporter.booking_location (id),
    estimated_distance double precision NOT NULL,
    estimated_finish_time timestamp with time zone
);

INSERT INTO atlas_transporter.one_way_booking (booking_id, to_location_id, estimated_distance, estimated_finish_time)
  SELECT T1.id, T1.to_location_id, T1.estimated_distance, T1.start_time
  FROM atlas_transporter.booking AS T1
  WHERE T1.fare_product_type = 'ONE_WAY';

ALTER TABLE atlas_transporter.booking DROP COLUMN IF EXISTS to_location_id;
ALTER TABLE atlas_transporter.booking DROP COLUMN IF EXISTS estimated_distance;
