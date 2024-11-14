-- {"api":"GetVolunteerBooking","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP VOLUNTEER VOLUNTEER_BOOKING_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/VOLUNTEER/GET_VOLUNTEER_BOOKING' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'VOLUNTEER' AND T1.user_action_type = 'VOLUNTEER_BOOKING_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"PostVolunteerAssignStartOtpRide","migration":"endpoint","param":"VolunteerAPI AssignCreateAndStartOtpRideEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/VOLUNTEER/POST_VOLUNTEER_ASSIGN_START_OTP_RIDE'
  WHERE endpoint = 'VolunteerAPI AssignCreateAndStartOtpRideEndpoint';

-- {"api":"PostVolunteerAssignStartOtpRide","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP VOLUNTEER VOLUNTEER_ASSIGN_CREATE_AND_START_OTP_RIDE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/VOLUNTEER/POST_VOLUNTEER_ASSIGN_START_OTP_RIDE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'VOLUNTEER' AND T1.user_action_type = 'VOLUNTEER_ASSIGN_CREATE_AND_START_OTP_RIDE' ) ON CONFLICT DO NOTHING;
