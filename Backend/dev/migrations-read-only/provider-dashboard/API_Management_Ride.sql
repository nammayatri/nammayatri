-- {"api":"PostRideEndMultiple","migration":"endpoint","param":"RideAPI MultipleRideEndEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideEndEndpoint';

-- {"api":"PostRideEndMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideEndMultipleEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideCancelEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideCancelMultipleEndpoint';

-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI RideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI RideSyncEndpoint';

-- {"api":"PostRideSync","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI PostRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpoint","param":"RideAPI MultipleRideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideSyncMultipleEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpoint","param":"RideAPI TicketRideListEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI TicketRideListEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI GetRideKaptureListEndpoint';
