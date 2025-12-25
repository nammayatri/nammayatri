-- ONLY FOR MASTER (DON'T RUN IN PRODUCTION) --
update atlas_driver_offer_bpp.fare_product set search_source = 'MOBILE_APP'
where merchant_operating_city_id = '7a0a3891-5945-4f86-a2f2-90c89c197c56' and area = 'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444' and vehicle_variant in ('SEDAN', 'HATCHBACK');

insert into atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, merchant_operating_city_id, area, trip_category, search_source, enabled, time_bounds)
select md5(random()::text || clock_timestamp()::text)::uuid, merchant_id, fare_policy_id, vehicle_variant, merchant_operating_city_id, area, 'OneWay_OneWayRideOtp', 'DASHBOARD', enabled, time_bounds from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id = '7a0a3891-5945-4f86-a2f2-90c89c197c56' and area = 'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444' and vehicle_variant in ('SEDAN', 'HATCHBACK');
