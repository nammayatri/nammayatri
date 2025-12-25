--ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN distance double precision NOT NULL;

-- ride
ALTER TABLE atlas_driver_offer_bpp.ride
  ALTER COLUMN fare SET DATA TYPE integer
  USING round(fare);

ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_booking_id FOREIGN KEY (booking_id) REFERENCES atlas_driver_offer_bpp.booking(id);
ALTER TABLE atlas_driver_offer_bpp.ride OWNER TO atlas_driver_offer_bpp_user;
