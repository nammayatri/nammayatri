-- {"api":"PostMapsAutoComplete","migration":"endpoint","param":"MapAPI AutoCompleteEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'
  WHERE endpoint = 'MapAPI AutoCompleteEndPoint';

-- {"api":"PostMapsAutoComplete","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS AUTOCOMPLETE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'AUTOCOMPLETE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMapsGetPlaceName","migration":"endpoint","param":"MapAPI GetPlaceNameEndPoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'
  WHERE endpoint = 'MapAPI GetPlaceNameEndPoint';

-- {"api":"PostMapsGetPlaceName","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP DRIVERS PLACENAME","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'DRIVERS' AND T1.user_action_type = 'PLACENAME' ) ON CONFLICT DO NOTHING;
