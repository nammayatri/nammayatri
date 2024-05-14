-- ONLY for Master (clear the cache too for vehicle service tier)
update atlas_driver_offer_bpp.vehicle_service_tier set air_conditioned = 1 where air_conditioned = 3;

-- ONLY for Production
update atlas_driver_offer_bpp.vehicle_service_tier set air_conditioned = 2 where air_conditioned = 3;