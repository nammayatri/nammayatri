CREATE INDEX IF NOT EXISTS idx_subscription_purchase_owner ON atlas_driver_offer_bpp.subscription_purchase USING btree (owner_id);
CREATE INDEX IF NOT EXISTS idx_subscription_purchase_owner_status ON atlas_driver_offer_bpp.subscription_purchase USING btree (owner_id, owner_type, status);
CREATE INDEX IF NOT EXISTS idx_subscription_purchase_payment_order_id ON atlas_driver_offer_bpp.subscription_purchase USING btree (payment_order_id);
