-- {"api":"PostMapsAutoComplete","migration":"endpoint","param":"MapsAPI AutoCompleteEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'
  WHERE endpoint = 'MapsAPI AutoCompleteEndPoint';

-- {"api":"PostMapsAutoComplete","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS AUTOCOMPLETE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'AUTOCOMPLETE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMapsGetPlaceDetails","migration":"endpoint","param":"MapsAPI GetPlaceDetailsEndPoints","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_DETAILS'
  WHERE endpoint = 'MapsAPI GetPlaceDetailsEndPoints';

-- {"api":"PostMapsGetPlaceDetails","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS PLACEDETAIL","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_DETAILS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'PLACEDETAIL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMapsGetPlaceName","migration":"endpoint","param":"MapsAPI GetPlaceNameEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'
  WHERE endpoint = 'MapsAPI GetPlaceNameEndPoint';

-- {"api":"PostMapsGetPlaceName","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS PLACENAME","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'PLACENAME' ) ON CONFLICT DO NOTHING;
