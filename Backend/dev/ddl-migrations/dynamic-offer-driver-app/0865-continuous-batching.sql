alter table atlas_driver_offer_bpp.driver_pool_config add column if not exists batching_mode text;
alter table atlas_driver_offer_bpp.driver_pool_config add column if not exists next_batch_schedule_time integer;
alter table atlas_driver_offer_bpp.search_try add column if not exists batching_mode text;
alter table atlas_driver_offer_bpp.search_request_for_driver add column if not exists batching_mode text;
