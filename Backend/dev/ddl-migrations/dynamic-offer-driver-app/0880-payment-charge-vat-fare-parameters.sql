-- Splits the Stripe payment charge from its VAT on fare_parameters.
--
-- payment_processing_fee previously held the VAT-INCLUSIVE blended amount
-- (rate x (1 + vat/100)). From this deploy it holds the NET charge and the VAT
-- lands here. Existing rows are deliberately NOT rewritten: a legacy row is
-- (gross, NULL) and a new row is (net, vat), and both sum to the same total, so
-- fareSum and every partition total stay correct either way. Only the split
-- differs, and only on invoices for rides priced before this deploy. Readers
-- must treat a NULL vat as "legacy blended".
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN payment_processing_fee_vat double precision;
