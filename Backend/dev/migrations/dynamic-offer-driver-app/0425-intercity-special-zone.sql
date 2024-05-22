INSERT INTO atlas_driver_offer_bpp.fare_product (
    area,
    enabled,
    fare_policy_id,
    id,
    merchant_id,
    merchant_operating_city_id,
    time_bounds,
    trip_category,
    vehicle_variant
)
SELECT
    'Pickup_9h8016d0-f9cd-4f9f-886f-bc4cbh6a86e5', -- special_location id
    true,
    fp.fare_policy_id,
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'Unbounded',
    'InterCity_OneWayOnDemandStaticOffer',
    'SUV'
FROM atlas_driver_offer_bpp.merchant_operating_city m INNER JOIN atlas_driver_offer_bpp.fare_product fp ON fp.vehicle_variant = 'SUV' AND m.city = 'Bangalore';


INSERT INTO atlas_driver_offer_bpp.fare_product (
    area,
    enabled,
    fare_policy_id,
    id,
    merchant_id,
    merchant_operating_city_id,
    time_bounds,
    trip_category,
    vehicle_variant
)
SELECT
    'Pickup_9h8016d0-f9cd-4f9f-886f-bc4cbh6a86e5', -- special_location id
    true,
    fp.fare_policy_id,
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'Unbounded',
    'InterCity_OneWayOnDemandStaticOffer',
    'AUTO_RICKSHAW'
FROM atlas_driver_offer_bpp.merchant_operating_city m INNER JOIN atlas_driver_offer_bpp.fare_product fp ON fp.vehicle_variant = 'AUTO_RICKSHAW' AND m.city = 'Bangalore';
