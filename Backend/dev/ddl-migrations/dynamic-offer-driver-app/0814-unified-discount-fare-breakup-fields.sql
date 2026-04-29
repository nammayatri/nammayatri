-- Unified Discount & VAT refactor: canonical ten-slot ProjectFareParamsBreakup on FareParameters.
--
-- Adds nine slot columns that partition the ride fare into:
--   - discount-applicable ride × {taxExcl, tax}
--   - non-discount-applicable ride × {taxExcl, tax}
--   - toll × {taxExcl, tax}
--   - cancellation × {taxExcl, tax}
--   - parking × {taxExcl, tax}
--
-- `toll_vat` already exists (from migration 0400) and is reused as the toll-tax slot;
-- the domain keeps the old `tollFareTax` name but maps to/from `toll_vat` in Storage.Queries.FareParameters
-- for backward compatibility with existing rows.

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN discount_applicable_ride_fare_tax_exclusive double precision,
ADD COLUMN discount_applicable_ride_fare_tax double precision,
ADD COLUMN non_discount_applicable_ride_fare_tax_exclusive double precision,
ADD COLUMN non_discount_applicable_ride_fare_tax double precision,
ADD COLUMN toll_fare_tax_exclusive double precision,
ADD COLUMN cancellation_fee_tax_exclusive double precision,
ADD COLUMN cancellation_tax double precision,
ADD COLUMN parking_charge_tax_exclusive double precision,
ADD COLUMN parking_charge_tax double precision;
