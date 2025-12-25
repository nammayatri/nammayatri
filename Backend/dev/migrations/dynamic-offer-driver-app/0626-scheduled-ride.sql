INSERT INTO atlas_driver_offer_bpp.fare_product (
    area,
    enabled,
    fare_policy_id,
    id,
    merchant_id,
    merchant_operating_city_id,
    time_bounds,
    trip_category,
    vehicle_variant,
    search_source,
    disable_recompute
)
SELECT
    area,
    enabled,
    fare_policy_id,
    md5(random()::text || clock_timestamp()::text)::uuid,
    merchant_id,
    merchant_operating_city_id,
    time_bounds,
    'OneWay_OneWayOnDemandStaticOffer',
    vehicle_variant,
    search_source,
    disable_recompute
FROM atlas_driver_offer_bpp.fare_product
WHERE trip_category = 'OneWay_OneWayOnDemandDynamicOffer' and merchant_operating_city_id in (SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Delhi' and merchant_short_id = 'MSIL_PARTNER');
