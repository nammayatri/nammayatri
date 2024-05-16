UPDATE atlas_app.merchant_service_usage_config SET enable_dashboard_sms=false;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN enable_dashboard_sms SET NOT NULL;

UPDATE atlas_app.merchant_service_usage_config SET enable_dashboard_sms = true
  WHERE merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51'; -- change merchant id to merchant id of yatri sathi

-- INSERT INTO atlas_app.merchant_message (merchant_id, message_key, message) VALUES
--     ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'SEND_BOOKING_OTP',
-- 'Dear User,

-- Please find your trip details below:

-- Ride OTP : {#otp#}
-- Ride Amount : {#amount#}

-- - Juspay');  -- change merchant id to merchant id of yatri sathi