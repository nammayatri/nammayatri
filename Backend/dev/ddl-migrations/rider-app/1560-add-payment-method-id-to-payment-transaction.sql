-- Provider payment method id actually used for a transaction, so the row keeps a historical
-- reference to it after the customer changes their default payment method.
-- Populated on the Stripe path only; Juspay exposes no equivalent identifier.
ALTER TABLE atlas_app.payment_transaction ADD COLUMN payment_method_id text;
