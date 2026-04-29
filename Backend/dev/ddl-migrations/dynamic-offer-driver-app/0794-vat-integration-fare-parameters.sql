-- VAT Integration: FareParameters changes
-- Remove rideVat (merged into govtCharges), add isVatTaxType boolean flag

ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN IF EXISTS ride_vat;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN is_vat_tax_type boolean;
