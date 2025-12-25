-- {"api":"PostBookingStatus","migration":"endpoint","param":"RBooking RideStatusEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS'
  WHERE endpoint = 'RBooking RideStatusEndPoint';

-- {"api":"PostBookingStatus","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS BOOKING_STATUS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'BOOKING_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetBookingList","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS BOOKINGLIST","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_LIST' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'BOOKINGLIST' ) ON CONFLICT DO NOTHING;
