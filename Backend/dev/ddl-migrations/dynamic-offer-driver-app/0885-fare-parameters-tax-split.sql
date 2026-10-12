-- Stores commission, cancellation commission and the driver-borne payment charge as (tax-exclusive, tax) pairs
-- on fare_parameters, written once when the row is created by calculateFareParameters.
-- Historical readers use these pairs instead of re-splitting with today's config.
-- The driver-borne payment charge pair is pre-discount (like payment_processing_fee); readers apply its tax ratio
-- to ride/booking.payment_charge, which stays the gross post-discount amount.
-- Existing rows are not backfilled: NULL pairs are read as (ride/booking gross amount, 0 tax).
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN commission_tax_exclusive double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN commission_tax double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN cancellation_commission_tax_exclusive double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN cancellation_commission_tax double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN driver_payment_charge_tax_exclusive double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN driver_payment_charge_tax double precision;

-- Untaxed ride fare parts, kept out of the taxed ride pairs so every taxed pair carries exactly one rate
-- (tax / tax-exclusive is the configured rate and can be back-calculated by invoices, refunds and discounts).
-- Carried-forward customer cancellation dues are gross and untaxed, so they land in the non-discount-applicable
-- untaxed part.
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN discount_applicable_ride_fare_untaxed double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN non_discount_applicable_ride_fare_untaxed double precision;
