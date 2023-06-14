CREATE TABLE atlas_driver_offer_bpp.ride_deviations
(   id character(36) NOT NULL,
    number_of_deviation int,
    CONSTRAINT ride_id_fkey FOREIGN KEY (id) REFERENCES atlas_driver_offer_bpp.ride(id)
);
