-- Pull-pending-doc-verification: the no-RC /vehicleStatus path resolves a pending row by registration
-- number (document_number_hash), which has no index on either verification table (full scan otherwise).
-- Driver-keyed pulls are already served by idx_idfy_verification_driver_id_doc_type / idx_hv_verification_driver_id.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_idfy_verification_document_number_hash ON atlas_driver_offer_bpp.idfy_verification USING btree (document_number_hash);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_hv_verification_document_number_hash ON atlas_driver_offer_bpp.hyperverge_verification USING btree (document_number_hash);
