-- NOTE: Only for Local, DON'T RUN IN MASTER OR PROD
--------------------------------------------------------------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.transporter_config SET ride_duration_diff_threshold_in_minutes = 0;
UPDATE atlas_driver_offer_bpp.transporter_config SET ride_duration_upwards_recompute_buffer_in_minutes = 5;
UPDATE atlas_driver_offer_bpp.transporter_config SET ride_duration_downwards_recompute_buffer_in_minutes = 5;
------------------------------------------------------------------* END *-------------------------------------------------------------------------