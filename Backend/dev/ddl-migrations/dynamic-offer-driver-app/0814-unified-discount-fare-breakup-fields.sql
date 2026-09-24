-- NOTE: dont need to run these queries
-- Unified Discount & VAT refactor: canonical ten-slot ProjectFareParamsBreakup on FareParameters.
--
-- Adds nine slot columns that partition the ride fare into:
--   - discount-applicable ride x {taxExcl, tax}
--   - non-discount-applicable ride x {taxExcl, tax}
--   - toll x {taxExcl, tax}
--   - cancellation x {taxExcl, tax}
--   - parking x {taxExcl, tax}
--
-- `toll_vat` already exists (from migration 0400) and is reused as the toll-tax slot;
-- the domain keeps the old `tollFareTax` name but maps to/from `toll_vat` in Storage.Queries.FareParameters
-- for backward compatibility with existing rows.

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS discount_applicable_ride_fare_tax_exclusive double precision,
ADD COLUMN IF NOT EXISTS discount_applicable_ride_fare_tax double precision,
ADD COLUMN IF NOT EXISTS non_discount_applicable_ride_fare_tax_exclusive double precision,
ADD COLUMN IF NOT EXISTS non_discount_applicable_ride_fare_tax double precision,
ADD COLUMN IF NOT EXISTS toll_fare_tax_exclusive double precision,
ADD COLUMN IF NOT EXISTS cancellation_fee_tax_exclusive double precision,
ADD COLUMN IF NOT EXISTS cancellation_tax double precision,
ADD COLUMN IF NOT EXISTS parking_charge_tax_exclusive double precision,
ADD COLUMN IF NOT EXISTS parking_charge_tax double precision;
-- NOTE: dont need to run these queries
