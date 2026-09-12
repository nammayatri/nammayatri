-- {"api":"PostRegistrationAuth","migration":"endpoint","param":"RegistrationAPI RegistrationAuthEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH'
  WHERE endpoint = 'RegistrationAPI RegistrationAuthEndPoint';

-- {"api":"PostRegistrationVerify","migration":"endpoint","param":"RegistrationAPI RegistrationVerifyEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY'
  WHERE endpoint = 'RegistrationAPI RegistrationVerifyEndPoint';

-- {"api":"PostRegistrationOtpResend","migration":"endpoint","param":"RegistrationAPI RegistrationResendEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_OTP_RESEND'
  WHERE endpoint = 'RegistrationAPI RegistrationResendEndPoint';

-- {"api":"PostRegistrationLogout","migration":"endpoint","param":"RegistrationAPI RegistrationLogoutEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_LOGOUT'
  WHERE endpoint = 'RegistrationAPI RegistrationLogoutEndPoint';
