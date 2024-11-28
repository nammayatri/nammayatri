INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active, vehicle_category)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'MetroRideCompleted ToMetro',
    'EndRide',
    city.merchant_id,
    city.id,
    0,
    12960000,
    False,
    'AUTO_CATEGORY'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active, vehicle_category)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'MetroRideCompleted FromMetro',
    'EndRide',
    city.merchant_id,
    city.id,
    0,
    12960000,
    False,
    'AUTO_CATEGORY'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;