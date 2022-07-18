CREATE TABLE atlas_driver_offer_bpp.fare_parameters (
  id character(36) NOT NULL PRIMARY KEY,
  fare_for_pickup double precision NOT NULL,
  distance_fare double precision NOT NULL,
  driver_selected_fare double precision,
  night_shift_rate double precision,
  night_coef_included boolean NOT NULL
);

-- driver quote
ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN base_fare;
ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN extra_fare_selected;

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_parameters_id character(36) NOT NULL REFERENCES fare_parameters(id);

-- ride
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP;

-- ride booking
ALTER TABLE atlas_driver_offer_bpp.ride_booking DROP COLUMN estimated_fare;

ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN fare_parameters_id character(36) NOT NULL REFERENCES fare_parameters(id);

-- search request
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN fare_parameters_id character(36) NOT NULL REFERENCES fare_parameters(id);
