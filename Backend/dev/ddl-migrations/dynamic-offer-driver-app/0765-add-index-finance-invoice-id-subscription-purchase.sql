-- Add indices for finance invoice query performance

-- subscription_purchase: index on finance_invoice_id for findByFinanceInvoiceId
CREATE INDEX idx_subscription_purchase_finance_invoice_id ON atlas_driver_offer_bpp.subscription_purchase USING btree (finance_invoice_id);

-- indirect_tax_transaction: index on invoice_number for findByInvoiceNumber
CREATE INDEX idx_indirect_tax_transaction_invoice_number ON atlas_driver_offer_bpp.indirect_tax_transaction USING btree (invoice_number);

-- finance_invoice: index on issued_to_id for findByIssuedToAndType
CREATE INDEX idx_finance_invoice_issued_to_id ON atlas_driver_offer_bpp.finance_invoice USING btree (issued_to_id);

-- finance_invoice: index on supplier_id for findBySupplierAndType
CREATE INDEX idx_finance_invoice_supplier_id ON atlas_driver_offer_bpp.finance_invoice USING btree (supplier_id);
