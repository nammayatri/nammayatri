-- Charge for the rider add-ons selected at /select (pricePerQuantity x selectedQuantity),
-- priced from add_on_config at selection and frozen here; part of the fare sum and its own
-- ADD_ON_CHARGES breakup line.
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN add_on_charges double precision;
