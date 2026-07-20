ALTER TABLE atlas_app.finance_reconciliation_entry
        ADD CONSTRAINT finance_reconciliation_entry_natural_key
        UNIQUE (domain, source, target, merchant_id, merchant_operating_city_id, entry_key);

CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_open_pool
    ON atlas_app.finance_reconciliation_entry
    USING btree (domain, source, target, merchant_id, merchant_operating_city_id, first_seen_at)
    WHERE open;

CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_summary_id
    ON atlas_app.finance_reconciliation_entry
    USING btree (summary_id);

CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_spec_source_record
    ON atlas_app.finance_reconciliation_entry
    USING btree (domain, source, target, source_record_id);
