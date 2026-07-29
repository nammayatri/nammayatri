-- EasyBooking has zero fare_product rows in the master fixture data (verified: not even its
-- existing OnDemandStaticOffer mode is seeded). Without a row here, getAllFareProducts /
-- getAllFarePoliciesProduct resolve zero fare policies for the EasyBooking_RideOtp candidate
-- and no quote is ever produced at the special zone below, regardless of code changes.
--
-- Reuses:
--   - the real Bengaluru airport gate (Kempegowda International, is_queue_enabled = true),
--     the same one already used by the existing Rental_RideOtp rows
--   - an existing Progressive-type fare_policy for AUTO_RICKSHAW on the same merchant/city,
--     since EasyBooking prices on the same Regular/Progressive formula as OneWay
--
-- Fixed id (not gen_random_uuid()) + ON CONFLICT DO NOTHING so this stays idempotent across
-- repeated config-sync imports; fare_product has no unique constraint besides the id PK.
INSERT INTO atlas_driver_offer_bpp.fare_product
    (id, area, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, time_bounds, trip_category, vehicle_variant, search_source, disable_recompute)
VALUES
    ('c716c374-4844-4d2c-b771-8f60e9227f8c', 'Pickup_b4365d87-1242-475b-b615-ce25d1172b5b', true, '170a4dd6-f3e6-4bbf-86da-6c3828979997', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', 'Unbounded', 'EasyBooking_RideOtp', 'AUTO_RICKSHAW', 'ALL', false)
ON CONFLICT (id) DO NOTHING;
