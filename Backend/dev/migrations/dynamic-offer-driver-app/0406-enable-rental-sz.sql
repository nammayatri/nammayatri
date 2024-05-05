INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, area, trip_category, merchant_operating_city_id, enabled, time_bounds)
    (SELECT
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.fare_policy_id,
        T1.vehicle_variant,
        T1.area,
        'Rental_OnDemandStaticOffer',
        T1.merchant_operating_city_id,
        T1.enabled,
        T1.time_bounds
    FROM atlas_driver_offer_bpp.fare_product AS T1 where T1.area != 'Default' and merchant_operating_city_id in ('1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', '7a0a3891-5945-4f86-a2f2-90c89c197c56'))
    ON CONFLICT DO NOTHING;
