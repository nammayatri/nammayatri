-- AUTO_RICKSHAW had no fare_product rows for Kempegowda International Airport's
-- special zone (Rental_RideOtp trip category), unlike COMFY/ECO/SUV/SUV_PLUS
-- which all have full coverage across the zone and every gate. As a result,
-- "Get Rental Airport Quotes" in NY Rental Airport OTP Flow could never return
-- an AUTO_RICKSHAW quote there, so the test's estimate-selection script fell
-- back to whatever else was available (SUV), creating a booking for the wrong
-- vehicle variant — the auto-rickshaw driver waiting in the queue then failed
-- "Driver OTP Ride Start" with "Wrong Vehicle Variant" (correct rejection,
-- wrong booking was made in the first place).
--
-- Reuses AUTO_RICKSHAW's existing Rental fare_policy (fd2ee856-...), already
-- used for its Default/Rental_OnDemandStaticOffer product — it's a proper
-- Rental-type policy so it's valid for the Rental_RideOtp booking flow too,
-- just not yet linked to the airport zone. Uses Unbounded time_bounds (one
-- row covering all hours) rather than mirroring SUV's peak/off-peak 3-way
-- split, since the test only needs a valid quote to exist, not differential
-- time-of-day pricing.

INSERT INTO atlas_driver_offer_bpp.fare_product
  (id, area, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, time_bounds, trip_category, vehicle_variant, search_source)
SELECT
  gen_random_uuid()::text, v.area, true, 'fd2ee856-7052-4a25-8d51-104da897f502',
  '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
  'Unbounded', 'Rental_RideOtp', 'AUTO_RICKSHAW', 'ALL'
FROM (
  VALUES
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_40b6a68c-c2f8-450f-b4ac-ab851c90ab7c'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_a3ec0b78-dd01-4722-b8ce-e6e485a87eab'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_c129bb63-c9e1-42e3-8a53-3f9c60cbd025'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_c834d402-5cfe-43ed-8515-e3c79983dcfa'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_d7fd1d35-640b-411f-bcaf-90828e4525b9'),
    ('Pickup_b4365d87-1242-475b-b615-ce25d1172b5b_Gate_e2e9fd3a-5259-4f2b-b61b-6c7c7a7a058c')
) AS v(area)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.fare_product fp
  WHERE fp.merchant_operating_city_id = '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98'
    AND fp.vehicle_variant = 'AUTO_RICKSHAW'
    AND fp.trip_category = 'Rental_RideOtp'
    AND fp.area = v.area
);
