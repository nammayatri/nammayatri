-- =====================================================================
-- Comprehensive finance-kernel indexes
-- Covers all SecondaryKey columns from YAML specs that lack SQL indexes,
-- plus BRIN indexes on date/timestamp columns used in range queries.
-- =====================================================================

-- =========================
-- finance_account
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_account_counterparty_id ON atlas_app.finance_account USING btree (counterparty_id);

-- =========================
-- finance_audit_entry
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_audit_entry_entity_id ON atlas_app.finance_audit_entry USING btree (entity_id);

-- =========================
-- finance_invoice
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_invoice_invoice_number ON atlas_app.finance_invoice USING btree (invoice_number);
-- BRIN on issued_at for date range queries
CREATE INDEX IF NOT EXISTS idx_finance_invoice_issued_at_brin ON atlas_app.finance_invoice USING brin (issued_at);

-- =========================
-- finance_ledger_entry
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_ledger_entry_reference_id ON atlas_app.finance_ledger_entry USING btree (reference_id);
CREATE INDEX IF NOT EXISTS idx_finance_ledger_entry_settlement_id ON atlas_app.finance_ledger_entry USING btree (settlement_id);
-- BRIN on timestamp for ordering / range queries
CREATE INDEX IF NOT EXISTS idx_finance_ledger_entry_timestamp_brin ON atlas_app.finance_ledger_entry USING brin (timestamp);

-- =========================
-- finance_invoice_ledger_link
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_invoice_ledger_link_invoice_id ON atlas_app.finance_invoice_ledger_link USING btree (invoice_id);
CREATE INDEX IF NOT EXISTS idx_finance_invoice_ledger_link_ledger_entry_id ON atlas_app.finance_invoice_ledger_link USING btree (ledger_entry_id);

-- =========================
-- pg_payment_settlement_report
-- =========================
CREATE INDEX IF NOT EXISTS idx_pg_payment_settlement_report_order_id ON atlas_app.pg_payment_settlement_report USING btree (order_id);
CREATE INDEX IF NOT EXISTS idx_pg_payment_settlement_report_rrn ON atlas_app.pg_payment_settlement_report USING btree (rrn);
CREATE INDEX IF NOT EXISTS idx_pg_payment_settlement_report_reference_id ON atlas_app.pg_payment_settlement_report USING btree (reference_id);
CREATE INDEX IF NOT EXISTS idx_pg_payment_settlement_report_settlement_id ON atlas_app.pg_payment_settlement_report USING btree (settlement_id);
-- BRIN on txn_date for range queries
CREATE INDEX IF NOT EXISTS idx_pg_payment_settlement_report_txn_date_brin ON atlas_app.pg_payment_settlement_report USING brin (txn_date);

-- =========================
-- pg_payout_settlement_report
-- =========================
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_order_id ON atlas_app.pg_payout_settlement_report USING btree (order_id);
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_rrn ON atlas_app.pg_payout_settlement_report USING btree (rrn);
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_payout_request_id ON atlas_app.pg_payout_settlement_report USING btree (payout_request_id);
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_settlement_id ON atlas_app.pg_payout_settlement_report USING btree (settlement_id);
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_payout_customer_id ON atlas_app.pg_payout_settlement_report USING btree (payout_customer_id);
-- BRIN on txn_date for range queries
CREATE INDEX IF NOT EXISTS idx_pg_payout_settlement_report_txn_date_brin ON atlas_app.pg_payout_settlement_report USING brin (txn_date);

-- =========================
-- finance_reconciliation_entry
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_reconciliation_entry_summary_id ON atlas_app.finance_reconciliation_entry USING btree (summary_id);
CREATE INDEX IF NOT EXISTS idx_finance_reconciliation_entry_booking_id ON atlas_app.finance_reconciliation_entry USING btree (booking_id);
CREATE INDEX IF NOT EXISTS idx_finance_reconciliation_entry_dco_id ON atlas_app.finance_reconciliation_entry USING btree (dco_id);
-- BRIN on reconciliation_date for range queries
CREATE INDEX IF NOT EXISTS idx_finance_reconciliation_entry_reconciliation_date_brin ON atlas_app.finance_reconciliation_entry USING brin (reconciliation_date);

-- =========================
-- finance_reconciliation_summary
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_reconciliation_summary_reconciliation_date ON atlas_app.finance_reconciliation_summary USING brin (reconciliation_date);

-- =========================
-- finance_state_transition
-- =========================
CREATE INDEX IF NOT EXISTS idx_finance_state_transition_entity_id ON atlas_app.finance_state_transition USING btree (entity_id);
