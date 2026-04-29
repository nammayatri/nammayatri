ALTER TABLE atlas_app.payment_order ADD COLUMN payment_service_type text;
ALTER TABLE atlas_app.payment_order ADD COLUMN valid_till timestamp with time zone;