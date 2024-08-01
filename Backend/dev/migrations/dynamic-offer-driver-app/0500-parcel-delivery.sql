insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant)
values (
    md5(random()::text || clock_timestamp()::text)::uuid,
    'Delivery Bike',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    null,
    null,
    null,
    null,
    null,
    'Effortless Parcel Delivery',
    null,
    '{BIKE}',
    '{}',
    'DELIVERY_BIKE',
    now(),
    now(),
    '{BIKE}'
);
