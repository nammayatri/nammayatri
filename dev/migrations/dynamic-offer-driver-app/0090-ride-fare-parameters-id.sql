ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id);
