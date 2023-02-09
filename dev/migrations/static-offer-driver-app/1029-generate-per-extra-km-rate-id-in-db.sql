ALTER TABLE atlas_transporter.fare_policy_per_extra_km_rate DROP COLUMN id;
ALTER TABLE atlas_transporter.fare_policy_per_extra_km_rate ADD COLUMN id SERIAL PRIMARY KEY;