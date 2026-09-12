
-- {"api":"PostProfileUpdate","migration":"endpoint","param":"ProfileAPI UpdatePersonEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/PROFILE/POST_PROFILE_UPDATE'
  WHERE endpoint = 'ProfileAPI UpdatePersonEndPoint';
