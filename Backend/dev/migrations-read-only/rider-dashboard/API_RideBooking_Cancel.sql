-- {"api":"PostCancelBooking","migration":"endpoint","param":"CancelAPI RideBookingCancelEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/CANCEL/POST_CANCEL_BOOKING'
  WHERE endpoint = 'CancelAPI RideBookingCancelEndPoint';

-- {"api":"PostCancelBooking","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS CANCEL_BOOKING","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/CANCEL/POST_CANCEL_BOOKING' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CANCEL_BOOKING' ) ON CONFLICT DO NOTHING;
