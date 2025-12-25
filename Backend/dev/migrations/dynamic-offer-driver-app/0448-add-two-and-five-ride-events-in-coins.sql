INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'TwoRidesCompleted',
    'EndRide',
    city.merchant_id,
    city.id,
    10,
    null,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'FiveRidesCompleted',
    'EndRide',
    city.merchant_id,
    city.id,
    40,
    null,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

UPDATE atlas_driver_offer_bpp.coin_config SET coins = 50 where event_function = 'EightPlusRidesInOneDay';