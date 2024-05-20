ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD column customer_cancellation_dues double precision;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD column cancellation_dues double precision DEFAULT 0 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD column dispute_chances_used int DEFAULT 0 NOT NULL;

CREATE INDEX ON atlas_driver_offer_bpp.cancellation_charges USING btree (driver_id);
