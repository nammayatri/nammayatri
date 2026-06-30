CREATE INDEX idx_sap_journal_entry_batch_id ON atlas_driver_offer_bpp.sap_journal_entry USING btree (batch_id);
CREATE INDEX idx_sap_journal_entry_created_at ON atlas_driver_offer_bpp.sap_journal_entry USING btree (created_at);
