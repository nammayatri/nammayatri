ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN base_fare;
ALTER TABLE atlas_driver_offer_bpp.driver_quote DROP COLUMN extra_fare_selected;

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN fare_for_pickup double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_selected_fare double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN night_shift_rate double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN night_coef_included boolean NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_time timestamp with time zone DEFAULT CURRENT_TIMESTAMP;

ALTER TABLE atlas_driver_offer_bpp.ride_booking DROP COLUMN estimated_fare;

ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN fare_for_pickup double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN distance_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN driver_selected_fare double precision;
ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN night_shift_rate double precision;
ALTER TABLE atlas_driver_offer_bpp.ride_booking ADD COLUMN night_coef_included boolean NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN fare_for_pickup double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN distance_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN driver_selected_fare double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN night_shift_rate double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN night_coef_included boolean NOT NULL;
