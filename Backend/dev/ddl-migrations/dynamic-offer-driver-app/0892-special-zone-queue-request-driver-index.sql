-- special_zone_queue_request only had its primary key (id) index, so the dashboard audit
-- API (driver_id + created_at window, newest first) would degrade into a sequential scan
-- on a table that grows by one row per notified driver per trigger.

CREATE INDEX CONCURRENTLY IF NOT EXISTS special_zone_queue_request_idx_driver_id_created_at
  ON atlas_driver_offer_bpp.special_zone_queue_request USING btree (driver_id, created_at DESC);
