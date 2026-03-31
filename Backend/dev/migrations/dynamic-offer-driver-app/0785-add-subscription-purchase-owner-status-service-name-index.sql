CREATE INDEX IF NOT EXISTS idx_subscription_purchase_owner_status_service_name
  ON atlas_driver_offer_bpp.subscription_purchase USING btree (owner_id, owner_type, status, service_name);
