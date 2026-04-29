-- Add indexes for direct_tax_transaction and indirect_tax_transaction tables

-- B-tree indexes on invoice_number
CREATE INDEX IF NOT EXISTS idx_direct_tax_transaction_invoice_number ON atlas_driver_offer_bpp.direct_tax_transaction USING btree (invoice_number);
CREATE INDEX IF NOT EXISTS idx_indirect_tax_transaction_invoice_number ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (invoice_number);

-- B-tree indexes on counterparty_id
CREATE INDEX IF NOT EXISTS idx_direct_tax_transaction_counterparty_id ON atlas_driver_offer_bpp.direct_tax_transaction USING btree (counterparty_id);
CREATE INDEX IF NOT EXISTS idx_indirect_tax_transaction_counterparty_id ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (counterparty_id);

-- BRIN indexes on transaction_date
CREATE INDEX IF NOT EXISTS idx_direct_tax_transaction_transaction_date_brin ON atlas_driver_offer_bpp.direct_tax_transaction USING brin (transaction_date);
CREATE INDEX IF NOT EXISTS idx_indirect_tax_transaction_transaction_date_brin ON atlas_driver_offer_bpp.indirect_tax_transaction USING brin (transaction_date);
