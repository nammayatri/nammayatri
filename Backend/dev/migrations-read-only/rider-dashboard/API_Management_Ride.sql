-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI MultipleRideSyncRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI MultipleRideSyncRideEndpoint';
