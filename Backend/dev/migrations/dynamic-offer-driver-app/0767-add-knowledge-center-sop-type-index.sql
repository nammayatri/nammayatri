-- Index for findAllBySopType queries (table created by generator in migrations-read-only)
CREATE INDEX idx_knowledge_center_sop_type ON atlas_driver_offer_bpp.knowledge_center USING btree (sop_type);
