UPDATE atlas_driver_offer_bpp.fare_product AS fp
SET enable = FALSE
WHERE fp.merchant_operating_city_id IN (
  SELECT moc.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
  WHERE moc.city = 'Kolkata'
)
  AND fp.area <> 'Default'
  AND fp.vehicle_variant IN ('SEDAN', 'HATCHBACK');

UPDATE atlas_driver_offer_bpp.fare_product AS fp
SET vehicle_variant = 'SUV_PLUS'
WHERE fp.merchant_operating_city_id IN (
  SELECT moc.id
  FROM atlas_driver_offer_bpp.merchant_operating_city AS moc
  WHERE moc.city = 'Kolkata'
)
  AND fp.area <> 'default'
  AND fp.vehicle_variant = 'HERITAGE_CAB';


INSERT INTO atlas_driver_offer_bpp.vehicle_service_tier (
  -- auto-generated id,
  air_conditioned_threshold,
  driver_rating,
  long_description,
  luggage_capacity,
  merchant_id,
  merchant_operating_city_id,
  name,
  seating_capacity,
  short_description,
  vehicle_rating,
  service_tier_type,
  default_for_vehicle_variant,
  allowed_vehicle_variant,
  priority,
  auto_selected_vehicle_variant,
  ventilator,
  oxygen,
  is_air_conditioned,
  -- auto-generated : created_at,
  -- auto-generated : updated_at,
  is_rentals_enabled,
  is_intercity_enabled,
  vehicle_icon_url,
  vehicle_category,
  fare_addition_per_km_over_base_service_tier,
  base_vehicle_service_tier,
  stop_fcm_threshold,
  stop_fcm_suppress_count,
  schedule_booking_list_eligibility_tags
)
VALUES (
  2,                                  -- Air Conditioned Threshold
  NULL,                               -- Driver Rating
  NULL,                               -- Long Description
  NULL,                               -- Luggage Capacity
  '96dd7f78-787e-4a0b-8675-e9e6fe93bb8f',  -- Merchant ID
  '7a0a3891-5945-4f86-a2f2-90c89c197c56',  -- Merchant Operating City ID
  'Merged Cab',                       -- Name
  4,                                  -- Seating Capacity
  'AC Merged Cabs',                   -- Short Description
  NULL,                               -- Vehicle Rating
  'SUV_PLUS',                         -- Service Tier Type
  ARRAY['HATCHBACK','SEDAN','HERITAGE_CAB'],  -- Default For Vehicle Variant
  ARRAY['HATCHBACK','SEDAN','HERITAGE_CAB'],  -- Allowed Vehicle Variant
  -1,                                  -- Priority
  ARRAY['HATCHBACK','SEDAN','HERITAGE_CAB'],  -- Auto Selected Vehicle Variant
  NULL,                               -- Ventilator
  NULL,                               -- Oxygen
  true,                               -- Is Air Conditioned
  NULL,                               -- Is Rentals Enabled
  true,                               -- Is Intercity Enabled
  'https://assets.moving.tech/beckn/common/user/images/xl_plus_ac.png',  -- Vehicle Icon URL
  'CAR',                              -- Vehicle Category
  NULL,                               -- Fare Addition Per Km Over Base Service Tier
  false,                              -- Base Vehicle Service Tier
  3,                               -- Stop Fcm Threshold
  5,                               -- Stop Fcm Suppress Count
  NULL                                -- Schedule Booking List Eligibility Tags
);

