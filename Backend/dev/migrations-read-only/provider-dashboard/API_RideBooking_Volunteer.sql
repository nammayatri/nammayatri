
-- {"api":"PostVolunteerAssignStartOtpRide","migration":"endpoint","param":"VolunteerAPI AssignCreateAndStartOtpRideEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/VOLUNTEER/POST_VOLUNTEER_ASSIGN_START_OTP_RIDE'
  WHERE endpoint = 'VolunteerAPI AssignCreateAndStartOtpRideEndpoint';
