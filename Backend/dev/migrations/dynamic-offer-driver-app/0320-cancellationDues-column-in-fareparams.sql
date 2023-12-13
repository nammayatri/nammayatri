ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD column customer_cancellation_dues double precision DEFAULT 0 NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column cancellation_fee double precision DEFAULT 10 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column driver_distance_travelled_on_pickup_threshold_on_cancel bigint DEFAULT 500 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column driver_time_spent_on_pickup_threshold_on_cancel int DEFAULT 600 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column cancellation_fee_dispute_limit int DEFAULT 3 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column num_of_cancellations_allowed int DEFAULT 3 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column driver_distance_to_pickup_threshold_on_cancel int DEFAULT 50 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD column can_add_cancellation_fee boolean DEFAULT false NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD column cancellation_dues double precision DEFAULT 0 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD column dispute_chances_used int DEFAULT 0 NOT NULL;

CREATE TABLE atlas_driver_offer_bpp.cancellation_charges (
    id CHARACTER(36) PRIMARY KEY NOT NULL,
    driver_id CHARACTER(36) NOT NULL,
    ride_id CHARACTER(36),
    cancellation_charges int
);

CREATE INDEX ON atlas_driver_offer_bpp.cancellation_charges USING btree (driver_id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD column customer_cancellation_dues double precision DEFAULT 0 NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD column customer_cancellation_dues double precision DEFAULT 0 NOT NULL;
