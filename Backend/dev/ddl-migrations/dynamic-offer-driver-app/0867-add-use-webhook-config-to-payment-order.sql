-- Mirror of rider-app 1565. lib/payment's PaymentOrder type is shared by both platforms, so the
-- column must exist in atlas_driver_offer_bpp too even though only the rider app writes it today.
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS use_webhook_config boolean;
