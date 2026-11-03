-- email_delivery: a record's attempts are listed by owner, and provider delivery / bounce events are matched by the
-- provider's message id.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_email_delivery_owner_id
  ON atlas_driver_offer_bpp.email_delivery (owner_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_email_delivery_provider_message_id
  ON atlas_driver_offer_bpp.email_delivery (provider_message_id);
