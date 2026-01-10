-- Run only in local and master

UPDATE atlas_driver_offer_bpp.transporter_config
SET avg_speed_of_vehicle = (
    avg_speed_of_vehicle::jsonb || '{"erickshaw": 20}'::jsonb
);

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator , vehicle_category ,base_vehicle_service_tier,priority,vehicle_icon_url)
(select
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'E Rickshaw',
    m.merchant_id,
    m.id,
    null,
    null,
    null,
    null,
    null,
    'Budget + Green',
    null,
    '{E_RICKSHAW}',
    '{E_RICKSHAW}',
    'E_RICKSHAW',
    now(),
    now(),
    '{E_RICKSHAW}',
    null,
    null,
    'AUTO_CATEGORY',
    true,
    3,
    'https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-auto_electric-1762772369564.png'
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Kolkata');


INSERT INTO atlas_driver_offer_bpp.fare_policy (
  id,
  night_shift_start,
  night_shift_end,
  created_at,
  updated_at,
  min_allowed_trip_distance,
  max_allowed_trip_distance,
  service_charge,
  govt_charges,
  fare_policy_type,
  description,
  per_minute_ride_extra_time_charge,
  congestion_charge_multiplier,
  parking_charge,
  currency,
  service_charge_amount,
  congestion_charge,
  toll_charges,
  merchant_id,
  merchant_operating_city_id,
  distance_unit
)
SELECT
  atlas_driver_offer_bpp.uuid_generate_v4(),
  '22:00:00',
  '05:00:00',
  now(),
  now(),
  0,
  100000,
  null,
  null,
  'Progressive',
  'E Rickshaw Fare Policy Kolkata',
  null,
  null,
  null,
  'INR',
  null,
  null,
  null,
  m.merchant_id,
  m.id,
  'Meter'
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
WHERE m.city = 'Kolkata';

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
  search_source
)
SELECT
  'Default',
  true,
  fp.id,
  atlas_driver_offer_bpp.uuid_generate_v4(),
  m.merchant_id,
  m.id,
  'Unbounded',
  'OneWayOnDemandDynamicOffer',
  'E_RICKSHAW',
  'ALL'
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN atlas_driver_offer_bpp.fare_policy AS fp
  ON fp.merchant_id = m.merchant_id
  AND fp.merchant_operating_city_id = m.id
WHERE m.city = 'Kolkata'
AND fp.description = 'E Rickshaw Fare Policy Kolkata';