-- Add new JSON columns for configurable charge types (VAT, Commission, Toll Tax)
-- These columns store charge configuration that allows percentage or fixed charges
-- to be applied on specific fare components.
--
-- Purpose: Enables flexible charge configuration per fare policy without code changes.
-- Charges can be configured as percentages (e.g., "14%") or fixed amounts (e.g., "50"),
-- and can target specific fare components (e.g., RideFare, DeadKmFareComponent).

ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN vat_charge_config TEXT,
ADD COLUMN commission_charge_config TEXT,
ADD COLUMN toll_tax_charge_config TEXT;

-- Example JSON format for these columns:
-- Percentage charge: {"value":"14%","appliesOn":["RideFare","DeadKmFareComponent"]}
-- Fixed charge: {"value":"50","appliesOn":["RideFare"]}
--
-- Note: Component names must match FareChargeComponent enum values exactly
-- (e.g., "RideFare", "DeadKmFareComponent", "TollChargesComponent")

