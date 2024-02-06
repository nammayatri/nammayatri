ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN stop_location_id character varying(36);

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_otp text;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_value double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_value double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN start_odometer_reading_file_id character varying(36);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN end_odometer_reading_file_id character varying(36);