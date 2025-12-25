-- only for local --
UPDATE atlas_driver_offer_bpp.fare_product
SET trip_category = 'Delivery_OneWayOnDemandDynamicOffer'
WHERE vehicle_variant = 'DELIVERY_BIKE' AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer';

-- only for local --
update atlas_driver_offer_bpp.vehicle_service_tier
set allowed_vehicle_variant = '{BIKE,DELIVERY_BIKE}'
where service_tier_type = 'DELIVERY_BIKE';

-- only for local --
INSERT INTO atlas_driver_offer_bpp.exophone (
    id,
    primary_phone,
    backup_phone,
    call_service,
    exophone_type,
    is_primary_down,
    merchant_id,
    merchant_operating_city_id,
    created_at,
    updated_at
)
VALUES
    (
        atlas_driver_offer_bpp.uuid_generate_v4(),
        'ExoPhone_Sender',
        'ExoPhone',
        'Exotel',
        'CALL_DELIVERY_SENDER',
        FALSE,
        'favorit0-0000-0000-0000-00000favorit',
        'favorit0-0000-0000-0000-00000000city',
        NOW(),
        NOW()
    ),
    (
        atlas_driver_offer_bpp.uuid_generate_v4(),
        'ExoPhone_Receiver',
        'ExoPhone',
        'Exotel',
        'CALL_DELIVERY_RECEIVER',
        FALSE,
        'favorit0-0000-0000-0000-00000favorit',
        'favorit0-0000-0000-0000-00000000city',
        NOW(),
        NOW()
    );

-- run only for master so commented out --
-- With FareProducts as (
--   select
--     md5(random()::text || clock_timestamp()::text)::uuid,
--     T1.merchant_id,
--     T1.fare_policy_id,
--     'DELIVERY_BIKE',
--     T1.area,
--     T1.merchant_operating_city_id,
--     'Delivery_OneWayOnDemandDynamicOffer',
--     T1.time_bounds,
--     T1.enabled,
--     T1.search_source
--   from
--     atlas_driver_offer_bpp.fare_product as T1
--   where
--     vehicle_variant = 'BIKE'
--     and merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f'
-- )
-- insert into
--   atlas_driver_offer_bpp.fare_product (
--     id,
--     merchant_id,
--     fare_policy_id,
--     vehicle_variant,
--     area,
--     merchant_operating_city_id,
--     trip_category,
--     time_bounds,
--     enabled,
--     search_source
--   )
-- select
--   *
-- from
--   FareProducts;


-- run only for master so commented out --
-- insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator)
-- (select
--     md5(random()::text || clock_timestamp()::text)::uuid,
--     'Delivery Bike',
--     m.merchant_id,
--     m.id,
--     1,
--     null,
--     null,
--     null,
--     null,
--     'Quick Delivery',
--     null,
--     '{BIKE,DELIVERY_BIKE}',
--     '{DELIVERY_BIKE}',
--     'DELIVERY_BIKE',
--     now(),
--     now(),
--     '{DELIVERY_BIKE}',
--     null,
--     null
--     from atlas_driver_offer_bpp.merchant_operating_city as m where merchant_id = '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f');


-- RUN IN MASTER ONLY .... ONLY IN MASTER ... ONLY IN MASTER
UPDATE atlas_driver_offer_bpp.beckn_config
SET On_Search_Ttl_Sec = 300
WHERE Vehicle_Category = 'AUTO_RICKSHAW';

-- RUN IN MASTER and PROD
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'Up to 5Kg' where service_tier_type = 'DELIVERY_BIKE';