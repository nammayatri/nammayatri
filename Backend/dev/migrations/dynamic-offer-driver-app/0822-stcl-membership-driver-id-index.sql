CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_stcl_membership_driver_id ON atlas_driver_offer_bpp.stcl_membership USING btree (driver_id);
