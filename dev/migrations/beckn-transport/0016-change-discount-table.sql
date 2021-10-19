DELETE FROM atlas_transporter.fare_policy_discount;

ALTER TABLE atlas_transporter.fare_policy_discount
  ADD COLUMN from_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_transporter.fare_policy_discount
  ADD COLUMN to_date timestamp with time zone NOT NULL;

ALTER TABLE atlas_transporter.fare_policy_discount DROP COLUMN start_time;
ALTER TABLE atlas_transporter.fare_policy_discount DROP COLUMN end_time;