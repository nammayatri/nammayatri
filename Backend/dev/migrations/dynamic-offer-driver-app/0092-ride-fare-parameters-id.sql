ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_fare_parameters_id FOREIGN KEY (fare_parameters_id) REFERENCES atlas_driver_offer_bpp.fare_parameters(id);
