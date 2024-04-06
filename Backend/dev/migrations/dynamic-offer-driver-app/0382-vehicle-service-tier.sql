update atlas_driver_offer_bpp.vehicle set variant = 'SEDAN' where variant = 'TAXI_PLUS';

update atlas_driver_offer_bpp.vehicle_registration_certificate set vehicle_variant = 'SEDAN' where vehicle_variant = 'TAXI_PLUS';

delete from atlas_driver_offer_bpp.fare_product where vehicle_variant = 'TAXI_PLUS';

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN vehicle_service_tier text;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN vehicle_service_tier text;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN air_conditioned boolean;

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