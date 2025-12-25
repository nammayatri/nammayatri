-- for local only --
UPDATE atlas_driver_offer_bpp.transporter_config
SET arrival_time_buffer_of_vehicle = '{"deliverybike":60}'::json
where merchant_id='favorit0-0000-0000-0000-00000favorit'; -- replace it only for yathri_sathi merchant_id


-- run in master --
UPDATE atlas_driver_offer_bpp.transporter_config
SET arrival_time_buffer_of_vehicle = '{"deliverybike":600}'::json
where merchant_id='96dd7f78-787e-4a0b-8675-e9e6fe93bb8f'; -- replace it only for yathri_sathi merchant_id
