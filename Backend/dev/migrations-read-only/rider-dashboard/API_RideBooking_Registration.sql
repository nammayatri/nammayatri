-- {"api":"PostRegistrationAuth","migration":"endpoint","param":"RegistrationAPI RegistrationAuthEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH'
  WHERE endpoint = 'RegistrationAPI RegistrationAuthEndPoint';

-- {"api":"PostRegistrationAuth","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS AUTH","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'AUTH' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRegistrationVerify","migration":"endpoint","param":"RegistrationAPI RegistrationVerifyEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY'
  WHERE endpoint = 'RegistrationAPI RegistrationVerifyEndPoint';

-- {"api":"PostRegistrationVerify","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS VERIFY","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'VERIFY' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRegistrationOtpResend","migration":"endpoint","param":"RegistrationAPI RegistrationResendEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_OTP_RESEND'
  WHERE endpoint = 'RegistrationAPI RegistrationResendEndPoint';

-- {"api":"PostRegistrationOtpResend","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS RESEND","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_OTP_RESEND' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'RESEND' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRegistrationLogout","migration":"endpoint","param":"RegistrationAPI RegistrationLogoutEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_LOGOUT'
  WHERE endpoint = 'RegistrationAPI RegistrationLogoutEndPoint';

-- {"api":"PostRegistrationLogout","migration":"userActionType","param":"ApiAuth APP_BACKEND CUSTOMERS LOGOUT","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_LOGOUT' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'LOGOUT' ) ON CONFLICT DO NOTHING;
