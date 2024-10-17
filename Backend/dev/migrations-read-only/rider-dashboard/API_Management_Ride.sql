-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI MultipleRideSyncRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI MultipleRideSyncRideEndpoint';


------- SQL updates -------

-- {"api":"PostRideCancel","migration":"endpoint","param":"RideAPI MultipleRideCancelRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL'
  WHERE endpoint = 'RideAPI MultipleRideCancelRideEndpoint';


------- SQL updates -------

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelRideEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideCancelRideEndpoint';
