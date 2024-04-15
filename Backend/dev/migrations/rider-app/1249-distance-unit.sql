ALTER TABLE atlas_app.booking ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.booking ADD COLUMN distance_value double precision;
ALTER TABLE atlas_app.booking ADD COLUMN estimated_distance_value double precision;

ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_dist_to_pickup_value double precision;

ALTER TABLE atlas_app.driver_offer ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.driver_offer ADD COLUMN distance_to_pickup_value double precision;

ALTER TABLE atlas_app.estimate ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance_value double precision;

ALTER TABLE atlas_app.merchant ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.merchant ADD COLUMN edit_pickup_distance_threshold_value double precision;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_distance_threshold_from_pickup_value double precision;
ALTER TABLE atlas_app.merchant ADD COLUMN arrived_pickup_threshold_value double precision;

ALTER TABLE atlas_app.rental_details ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.rental_details ADD COLUMN included_distance_per_hr_value double precision;

ALTER TABLE atlas_app.search_request ADD COLUMN distance_unit character varying(255);
ALTER TABLE atlas_app.search_request ADD COLUMN distance_value double precision;
ALTER TABLE atlas_app.search_request ADD COLUMN max_distance_value double precision;
