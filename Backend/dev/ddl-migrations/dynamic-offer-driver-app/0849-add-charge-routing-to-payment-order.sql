-- Mirror of rider-app migration 1559. payment_order is backed by the shared lib/payment beam table,
-- so the column must exist in both schemas or the driver-app queries break on the shared row type.
--
-- Note this schema has payment_order in enableKVForRead and enableKVForWriteAlso at 100%, so the
-- column must exist BEFORE the code that writes it is deployed.
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS charge_routing text;
