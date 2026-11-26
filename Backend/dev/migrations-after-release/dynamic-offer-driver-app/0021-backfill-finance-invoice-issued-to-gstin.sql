-- Backfill finance_invoice.issued_to_gstin (recipient's GSTIN) for invoices created
-- before the column existed.
--
-- Run AFTER the release that adds the column and starts writing it on invoice creation.
--
-- Source: indirect_tax_transaction.gstin_of_party. At invoice creation that value was
-- taken from the recipient's record (fleet_owner_information.gst_number for fleet
-- owners), so it is the GSTIN as of the invoice date. The live fleet_owner_information
-- value cannot be used here: it is encrypted and may have changed since.
--
-- Idempotent: only fills rows that are still NULL.

UPDATE atlas_driver_offer_bpp.finance_invoice i
SET issued_to_gstin = t.gstin_of_party
FROM atlas_driver_offer_bpp.indirect_tax_transaction t
WHERE t.invoice_number = i.invoice_number
  AND t.gstin_of_party IS NOT NULL
  AND t.gstin_of_party <> ''
  AND i.issued_to_gstin IS NULL;
