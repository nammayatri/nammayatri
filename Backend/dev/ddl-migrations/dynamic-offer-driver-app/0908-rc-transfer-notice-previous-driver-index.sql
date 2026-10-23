-- rc_transfer_notice is read on every driver profile load by previous_driver_id (pending notice lookup);
-- without this it is a sequential scan once the KV cache misses.

CREATE INDEX CONCURRENTLY IF NOT EXISTS rc_transfer_notice_idx_previous_driver_id
  ON atlas_driver_offer_bpp.rc_transfer_notice (previous_driver_id);
