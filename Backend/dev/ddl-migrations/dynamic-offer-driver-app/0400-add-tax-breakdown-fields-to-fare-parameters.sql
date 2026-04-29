-- Add new fields to fare_parameters table for transparent tax breakdown
-- These fields store individual tax components for compliance and transparency.
-- This allows showing a detailed breakdown of charges to users (e.g., Finland requirements).
--
-- Note: These fields are populated by calculateFareParametersV2 based on
-- fare_policy charge configurations (vat_charge_config, commission_charge_config, toll_tax_charge_config).

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN payment_processing_fee double precision,
ADD COLUMN ride_vat double precision,
ADD COLUMN toll_vat double precision,
ADD COLUMN commission double precision;

-- Field descriptions:
-- payment_processing_fee: Payment processing fee (blended or method-specific)
--                        TODO: Will be enhanced when payment context is available
-- ride_vat: VAT charge calculated based on vat_charge_config in fare_policy
--           Example: 14% on (RideFare + CongestionChargeComponent)
-- toll_vat: VAT on toll charges calculated based on toll_tax_charge_config in fare_policy
--           Example: 25% on TollChargesComponent
-- commission: Commission calculated based on commission_charge_config in fare_policy
--             Example: 8% on RideFare
--             IMPORTANT: Commission is stored for breakdown but NOT included in final fare sum

