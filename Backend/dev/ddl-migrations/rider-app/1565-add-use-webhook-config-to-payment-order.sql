-- Stamped on a payment_order when the Juspay webhook for it arrived with useWebhookConfig=true.
-- Once set, every later gateway call for that order -- order status, refunds, and the scheduler
-- retries that reload the order -- resolves the dedicated WebhookPayment_Juspay merchant service
-- config instead of the one implied by payment_service_type (MultiModalPayment_/PassPayment_).
--
-- NULL is the normal case, which is what every existing row is.
--
-- Apply this in every environment BEFORE deploying the code that writes it, and make sure the
-- WebhookPayment_Juspay row exists in merchant_service_config before Juspay starts sending the flag.
ALTER TABLE atlas_app.payment_order ADD COLUMN IF NOT EXISTS use_webhook_config boolean;
