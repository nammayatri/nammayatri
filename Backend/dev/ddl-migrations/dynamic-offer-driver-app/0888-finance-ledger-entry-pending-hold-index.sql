CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_ledger_entry_idx_pending_holds
    ON atlas_driver_offer_bpp.finance_ledger_entry
    USING btree (from_account_id, reference_type)
    WHERE status = 'PENDING';
