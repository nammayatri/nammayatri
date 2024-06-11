ALTER TABLE atlas_driver_offer_bpp.quote_special_zone add column vehicle_service_tier_name text;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone add column driver_min_fee Int;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone add column driver_max_fee Int;
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone add column driver_pick_up_charge Int;

-- TAXI, ECO, COMFY, SUV
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 0 where name = 'SUV';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 1 where name = 'COMFY';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 1 where name = 'SEDAN';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 2 where name = 'ECO';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 2 where name = 'HATCHBACK';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 3 where name = 'TAXI';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 4 where name = 'AUTO_RICKSHAW';
update atlas_driver_offer_bpp.vehicle_service_tier set priority = 5 where name = 'BIKE';