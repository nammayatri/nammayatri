-- {"api":"PostDriverCoinsBulkUploadCoins","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_COIN_BULK_UPLOAD","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_COIN_BULK_UPLOAD' ) ON CONFLICT DO NOTHING;

-- {"api":"PostDriverCoinsBulkUploadCoinsV2","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_COIN_BULK_UPLOAD_V2","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS_V2' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_COIN_BULK_UPLOAD_V2' ) ON CONFLICT DO NOTHING;

-- {"api":"GetDriverCoinsCoinHistory","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_COIN_HISTORY","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_COINS/GET_DRIVER_COINS_COIN_HISTORY' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_COIN_HISTORY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostDriverCoinsBlacklistedEventsUpdate","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT DRIVERS DRIVER_COIN_BLACKLIST_EVENTS_UPDATE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BLACKLISTED_EVENTS_UPDATE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'DRIVER_COIN_BLACKLIST_EVENTS_UPDATE' ) ON CONFLICT DO NOTHING;
