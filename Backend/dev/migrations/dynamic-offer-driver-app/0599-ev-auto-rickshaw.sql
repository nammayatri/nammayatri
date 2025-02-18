-- Do not run anywhere --
insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator, is_air_conditioned, air_conditioned_threshold)
(select
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'ECO Auto',
    m.merchant_id,
    m.id,
    3,
    null,
    null,
    null,
    null,
    'EV Auto',
    null,
    '{EV_AUTO_RICKSHAW}',
    '{EV_AUTO_RICKSHAW}',
    'EV_AUTO_RICKSHAW',
    now(),
    now(),
    '{EV_AUTO_RICKSHAW}',
    null,
    null,
    false,
    null
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Trichy');

update atlas_driver_offer_bpp.vehicle_service_tier set vehicle_icon_url = 'https://assets.moving.tech/beckn/common/user/images/auto.png' where name = 'ECO Auto';