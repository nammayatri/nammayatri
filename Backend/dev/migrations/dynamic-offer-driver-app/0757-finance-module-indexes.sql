-- Note: CONCURRENTLY cannot be used inside a transaction block.
-- Migration runner must execute each statement individually outside of explicit transactions.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_issued_at ON atlas_driver_offer_bpp.finance_invoice USING brin (issued_at);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_supplier_issued_to ON atlas_driver_offer_bpp.finance_invoice USING btree (supplier_id, issued_to_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_invoice_type ON atlas_driver_offer_bpp.finance_invoice USING btree (invoice_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_invoice_number
    ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (invoice_number);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_transaction_order_id ON atlas_driver_offer_bpp.payment_transaction USING btree (order_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_to_account ON atlas_driver_offer_bpp.finance_ledger_entry USING btree (to_account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_from_account ON atlas_driver_offer_bpp.finance_ledger_entry USING btree (from_account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_timestamp ON atlas_driver_offer_bpp.finance_ledger_entry USING brin (timestamp);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_reference ON atlas_driver_offer_bpp.finance_ledger_entry USING btree (reference_type, reference_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_created_at ON atlas_driver_offer_bpp.payout_request USING brin (created_at);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_beneficiary_id ON atlas_driver_offer_bpp.payout_request USING btree (beneficiary_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_account_merchant_type
    ON atlas_driver_offer_bpp.finance_account USING btree (merchant_id, merchant_operating_city_id, account_type, counterparty_type);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_ledger_entry_created_at ON atlas_driver_offer_bpp.finance_ledger_entry USING brin (created_at);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_ledger_link_ledger_entry
    ON atlas_driver_offer_bpp.finance_invoice_ledger_link USING btree (ledger_entry_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_invoice_ledger_link_invoice
    ON atlas_driver_offer_bpp.finance_invoice_ledger_link USING btree (invoice_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_subscription_purchase_created_at ON atlas_driver_offer_bpp.subscription_purchase USING brin (created_at);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_subscription_purchase_merchant_city ON atlas_driver_offer_bpp.subscription_purchase USING btree (merchant_id, merchant_operating_city_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_subscription_purchase_finance_invoice ON atlas_driver_offer_bpp.subscription_purchase USING btree (finance_invoice_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_date ON atlas_driver_offer_bpp.indirect_tax_transaction USING brin (transaction_date);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_merchant_city
    ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (merchant_id, merchant_operating_city_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_indirect_tax_transaction_reference_id
    ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (reference_id);
