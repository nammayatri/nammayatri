CREATE INDEX IF NOT EXISTS idx_rr_entity_lookup ON atlas_driver_offer_bpp.review_request
  USING btree (entity_id, entity_type, request_type, request_status);
