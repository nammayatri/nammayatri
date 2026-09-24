-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD column IF NOT EXISTS customer_cancellation_dues double precision;

CREATE INDEX ON atlas_driver_offer_bpp.cancellation_charges USING btree (driver_id);
-- NOTE: dont need to run these queries
