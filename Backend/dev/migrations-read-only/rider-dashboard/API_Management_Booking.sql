-- {"api":"PostBookingCancelAllStuck","migration":"endpoint","param":"BookingAPI StuckBookingsCancelEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'
  WHERE endpoint = 'BookingAPI StuckBookingsCancelEndpoint';

-- {"api":"PostBookingCancelAllStuck","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'
  WHERE endpoint = 'BookingAPI PostBookingCancelAllStuckEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpoint","param":"BookingAPI MultipleBookingSyncEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'
  WHERE endpoint = 'BookingAPI MultipleBookingSyncEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'
  WHERE endpoint = 'BookingAPI PostBookingSyncMultipleEndpoint';


------- SQL updates -------

-- {"api":"PostBookingCancelAllStuck","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES STUCK_BOOKING_CANCEL","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'STUCK_BOOKING_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostBookingSyncMultiple","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT RIDES MULTIPLE_BOOKING_SYNC","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'MULTIPLE_BOOKING_SYNC' ) ON CONFLICT DO NOTHING;
