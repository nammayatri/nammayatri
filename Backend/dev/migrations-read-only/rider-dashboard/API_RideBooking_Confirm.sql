-- {"api":"PostConfirmRideSearchQuotes","migration":"endpoint","param":"ConfirmAPI ConfirmEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES'
  WHERE endpoint = 'ConfirmAPI ConfirmEndPoint';

-- {"api":"PostConfirmRideSearchQuotes","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS CONFIRM","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CONFIRM' ) ON CONFLICT DO NOTHING;
