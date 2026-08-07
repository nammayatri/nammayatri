-- Records how a gateway charge routed its funds, as actually applied at creation time.
-- NULL means a destination charge (transfer_data + application_fee_amount), which is what every
-- existing row is and what every non-Stripe order stays. 'PlatformOnlyCharge' means the whole
-- amount settled on the platform account, so capture and refund must omit application_fee_amount,
-- reverse_transfer and refund_application_fee.
--
-- Apply this in every environment BEFORE deploying the code that writes it.
ALTER TABLE atlas_app.payment_order ADD COLUMN IF NOT EXISTS charge_routing text;
