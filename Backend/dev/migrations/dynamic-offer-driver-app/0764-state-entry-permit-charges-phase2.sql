-- FareParameters: SEPC (parallel to toll_charges); FareParameters has no YAML here so we add the column manually.
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN state_entry_permit_charges numeric(30, 2);
