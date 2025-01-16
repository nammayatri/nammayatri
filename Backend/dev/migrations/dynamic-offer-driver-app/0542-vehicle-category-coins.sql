ALTER TABLE atlas_driver_offer_bpp.coin_config ADD COLUMN vehicle_category text;

ALTER TABLE atlas_driver_offer_bpp.coin_history ADD COLUMN vehicle_category text;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active, vehicle_category)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    config.event_function,
    config.event_name,
    config.merchant_id,
    config.merchant_opt_city_id,
    config.coins,
    config.expiration_at,
    FALSE,
    vehicle.vehicle_category
FROM
    (VALUES
        ('CAR'),
        ('AUTO_CATEGORY')
    ) AS vehicle(vehicle_category),
    atlas_driver_offer_bpp.coin_config AS config
WHERE config.vehicle_category IS NULL;