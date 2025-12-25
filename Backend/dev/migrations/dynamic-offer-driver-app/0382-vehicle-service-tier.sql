update atlas_driver_offer_bpp.vehicle set variant = 'SEDAN' where variant = 'TAXI_PLUS';

update atlas_driver_offer_bpp.vehicle_registration_certificate set vehicle_variant = 'SEDAN' where vehicle_variant = 'TAXI_PLUS';

delete from atlas_driver_offer_bpp.fare_product where vehicle_variant = 'TAXI_PLUS';

-- run in only in prod and master: commenting as local will break
-- ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ALTER COLUMN allowed_vehicle_variant TYPE text[] USING allowed_vehicle_variant::text[];

insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at)
(select
  md5(random()::text || clock_timestamp()::text)::uuid,
  vehicle_variant,
  merchant_id,
  merchant_operating_city_id,
  null,
  null,
  null,
  null,
  null,
  null,
  null,
  array[vehicle_variant],
  array[vehicle_variant],
  vehicle_variant,
  now(),
  now()
  from atlas_driver_offer_bpp.fare_product group by merchant_id, merchant_operating_city_id, vehicle_variant);

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Auto' where service_tier_type = 'AUTO_RICKSHAW';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Hatchback' where service_tier_type = 'HATCHBACK';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Sedan' where service_tier_type = 'SEDAN';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'SUV' where service_tier_type = 'SUV';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Non-AC' where service_tier_type = 'TAXI';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Bike Taxi' where service_tier_type = 'BIKE';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Bike' where service_tier_type = 'DELIVERY_BIKE';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Truck Mini' where service_tier_type = 'DELIVERY_TRUCK_MINI';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Truck Small' where service_tier_type = 'DELIVERY_TRUCK_SMALL';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Truck Medium' where service_tier_type = 'DELIVERY_TRUCK_MEDIUM';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Truck Large' where service_tier_type = 'DELIVERY_TRUCK_LARGE';
update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Delivery Truck Ultra Large' where service_tier_type = 'DELIVERY_TRUCK_ULTRA_LARGE';


update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'Commute friendly' where service_tier_type = 'AUTO_RICKSHAW';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'AC, Spacious rides' where service_tier_type = 'SUV';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'AC, Plush rides' where service_tier_type = 'SEDAN';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'AC, Budget rides' where service_tier_type = 'HATCHBACK';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'Commute friendly' where service_tier_type = 'BIKE';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = 'Quick Delivery' where service_tier_type = 'DELIVERY_BIKE';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = '350 Kg' where service_tier_type = 'DELIVERY_TRUCK_MINI';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = '500 Kg' where service_tier_type = 'DELIVERY_TRUCK_SMALL';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = '1000 Kg' where service_tier_type = 'DELIVERY_TRUCK_MEDIUM';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = '1500 Kg' where service_tier_type = 'DELIVERY_TRUCK_LARGE';
update atlas_driver_offer_bpp.vehicle_service_tier set short_description = '2500 Kg' where service_tier_type = 'DELIVERY_TRUCK_ULTRA_LARGE';

-- Backfill selected_service_tiers in vehicle
UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = array_remove(ARRAY[
    vehicle.variant, -- Always include the current variant
    CASE WHEN driver_information.can_downgrade_to_sedan THEN 'SEDAN' ELSE NULL END,
    CASE WHEN driver_information.can_downgrade_to_hatchback THEN 'HATCHBACK' ELSE NULL END,
    CASE WHEN driver_information.can_downgrade_to_taxi THEN 'TAXI' ELSE NULL END
]::text[], NULL) -- Ensure array is of type text[], removing any NULL values
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'SUV';

UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = array_remove(ARRAY[
    vehicle.variant,
    CASE WHEN driver_information.can_downgrade_to_hatchback THEN 'HATCHBACK' ELSE NULL END,
    CASE WHEN driver_information.can_downgrade_to_taxi THEN 'TAXI' ELSE NULL END
]::text[], NULL) -- Ensure array is of type text[], removing any NULL values
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'SEDAN';

UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = array_remove(ARRAY[
    vehicle.variant,
    CASE WHEN driver_information.can_downgrade_to_taxi THEN 'TAXI' ELSE NULL END
]::text[], NULL) -- Ensure array is of type text[], removing any NULL values
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'HATCHBACK';

UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = array_remove(ARRAY[
    vehicle.variant
]::text[], NULL) -- Ensure array is of type text[], removing any NULL values
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'TAXI';

UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = array_remove(ARRAY[
    vehicle.variant
]::text[], NULL) -- Ensure array is of type text[], removing any NULL values
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'AUTO_RICKSHAW';

UPDATE atlas_driver_offer_bpp.vehicle
SET selected_service_tiers = ARRAY['BIKE']::text[]
FROM atlas_driver_offer_bpp.driver_information as driver_information
WHERE vehicle.driver_id = driver_information.driver_id and vehicle.variant = 'BIKE';