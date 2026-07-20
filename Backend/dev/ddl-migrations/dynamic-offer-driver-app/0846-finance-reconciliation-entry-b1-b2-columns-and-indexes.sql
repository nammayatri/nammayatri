ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD CONSTRAINT finance_reconciliation_entry_natural_key UNIQUE (domain, source, target, merchant_id, merchant_operating_city_id, entry_key);

-- B2 sweep hot path: findOpenBySpecScope filters on the full identity
-- tuple + open, ordered by first_seen_at. Partial index on WHERE open
-- keeps the working set proportional to outstanding work rather than to
-- history — closed entries never appear in the sweep read.
CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_open_pool
    ON atlas_driver_offer_bpp.finance_reconciliation_entry
    USING btree (domain, source, target, merchant_id, merchant_operating_city_id, first_seen_at)
    WHERE open;

-- Dashboard entry-list read: findBySummaryIdWithPagination.
CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_summary_id
    ON atlas_driver_offer_bpp.finance_reconciliation_entry
    USING btree (summary_id);

-- Settlement-list enrichment: findBySpecAndSourceIds bulk-fetches on
-- (domain, source, target, source_record_id) — the settlement dashboard
-- attaches per-row recon status to each PG report row.
CREATE INDEX CONCURRENTLY IF NOT EXISTS finance_reconciliation_entry_idx_spec_source_record
    ON atlas_driver_offer_bpp.finance_reconciliation_entry
    USING btree (domain, source, target, source_record_id);
