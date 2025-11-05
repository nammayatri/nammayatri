insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator , vehicle_category ,vehicle_icon_url)
(select
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'Bike Metro',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Budget + Fast',
    null,
    '{BIKE,BIKE_PLUS}',
    '{BIKE_PLUS}',
    'BIKE_PLUS',
    now(),
    now(),
    '{BIKE,BIKE_PLUS}',
    null,
    null,
    'MOTORCYCLE',
    'https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-bike_metro-1-1762324617472.png'
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');



INSERT INTO atlas_driver_offer_bpp.fare_product (area,enabled,fare_policy_id,id,merchant_id,merchant_operating_city_id,time_bounds,trip_category,vehicle_variant)
SELECT 'Default', true, '81921560-3fd8-4594-acf4-8bfbd16caafc' , atlas_driver_offer_bpp.uuid_generate_v4(), m.merchant_id,m.id,'Unbounded','BIKEPLUS_OneWayOnDemandDynamicOffer','BIKE_PLUS' FROM atlas_driver_offer_bpp.merchant_operating_city as m where m.city = 'Kolkata';


UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = (
    avg_speed_of_vehicle::jsonb || '{"bikeplus": 40}'::jsonb
);