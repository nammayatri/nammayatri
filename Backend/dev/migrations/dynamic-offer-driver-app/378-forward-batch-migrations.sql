-------------------------- for local and master/ for prod enable and change configs on case to case basis -----------------
update atlas_driver_offer_bpp.transporter_config set include_driver_currently_on_ride = true from (select * from atlas_driver_offer_bpp.merchant_operating_city where city = 'KOCHI') as moc where merchant_operating_city_id = moc.id  ;
update atlas_driver_offer_bpp.driver_pool_config set enable_forward_batching = true ;
update atlas_driver_offer_bpp.driver_pool_config set on_ride_batch_split_config = '{"BatchSplitByPickupDistanceOnRide { batchSplitSize = 1, batchSplitDelay = 0 }","BatchSplitByPickupDistanceOnRide { batchSplitSize = 1, batchSplitDelay = 4 }"}';   ------ temporary change for testing
----------------------------------------------------------------------------------------------------------------------------
--alter table atlas_driver_offer_bpp.transporter_config add column use_silent_fcm_for_forward_batch boolean default false;