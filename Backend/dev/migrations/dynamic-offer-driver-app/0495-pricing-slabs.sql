CREATE TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);


CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs) AS id,
  id,
  0,
  0,
  10,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'InterCity';

INSERT INTO atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs) AS id,
  id,
  2,
  0,
  10,
  true,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'InterCity';

INSERT INTO atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs) AS id,
  id,
  2,
  75,
  100,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'InterCity';

INSERT INTO atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs) AS id,
  id,
  75,
  0,
  100,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'InterCity';

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs) AS id,
  id,
  0,
  0,
  10,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'Rental';

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs) AS id,
  id,
  2,
  0,
  10,
  true,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'Rental';

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs) AS id,
  id,
  2,
  75,
  100,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'Rental';

INSERT INTO atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs
(id, fare_policy_id, time_percentage, distance_percentage, fare_percentage, include_actual_time_percentage, include_actual_dist_percentage)
SELECT
  ROW_NUMBER() OVER() + (SELECT COALESCE(MAX(id), 0) FROM atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs) AS id,
  id,
  75,
  0,
  100,
  false,
  false
FROM
  atlas_driver_offer_bpp.fare_policy
WHERE
  fare_policy_type = 'Rental';

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_meters integer;

UPDATE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers SET buffer_meters = 500 where ride_duration = 0;
UPDATE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers SET buffer_meters = 1000 where ride_duration = 5;