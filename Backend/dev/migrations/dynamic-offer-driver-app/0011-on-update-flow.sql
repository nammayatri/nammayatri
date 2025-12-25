CREATE TABLE atlas_driver_offer_bpp.fare_parameters (
  id character(36) NOT NULL PRIMARY KEY,
  fare_for_pickup double precision NOT NULL,
  distance_fare double precision NOT NULL,
  driver_selected_fare double precision,
  night_shift_rate double precision,
  night_coef_included boolean NOT NULL
);

-- driver quote


