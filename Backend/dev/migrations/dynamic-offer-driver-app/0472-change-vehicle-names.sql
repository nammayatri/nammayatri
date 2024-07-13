update atlas_driver_offer_bpp.vehicle_service_tier set name = 'AC Mini', short_description = 'Budget + Cool' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'HATCHBACK';

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Non-AC Mini', short_description = 'Budget friendly' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'TAXI';

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Bike Taxi', short_description = 'Fast and frugal', seating_capacity = 1 where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'BIKE';

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'Sedan', short_description = 'Premium Comfort' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'SEDAN';

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'XL Cab', short_description = 'AC, Extra Spacious' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'SUV';

update atlas_driver_offer_bpp.vehicle_service_tier set name = 'XL Plus', short_description = 'AC, Extra Spacious' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Siliguri')) and service_tier_type = 'SUV_PLUS';
