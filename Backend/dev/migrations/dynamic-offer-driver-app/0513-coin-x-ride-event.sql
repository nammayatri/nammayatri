-- Only To be Run Master DB
INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 1',
    'EndRide',
    city.merchant_id,
    city.id,
    69,
    12960000,
    True
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;