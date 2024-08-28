-- {"api":"PostRideEndMultiple","migration":"endpoint","param":"RideAPI MultipleRideEndEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideEndMultipleEndpoint'
  WHERE endpoint = 'RideAPI MultipleRideEndEndpoint';

-- {"api":"PostRideEndMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideEndMultipleEndpoint'
  WHERE endpoint = 'RideAPI PostRideEndMultipleEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideCancelMultipleEndpoint'
  WHERE endpoint = 'RideAPI MultipleRideCancelEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideCancelMultipleEndpoint'
  WHERE endpoint = 'RideAPI PostRideCancelMultipleEndpoint';

-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI RideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideSyncEndpoint'
  WHERE endpoint = 'RideAPI RideSyncEndpoint';

-- {"api":"PostRideSync","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideSyncEndpoint'
  WHERE endpoint = 'RideAPI PostRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpoint","param":"RideAPI MultipleRideSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideSyncMultipleEndpoint'
  WHERE endpoint = 'RideAPI MultipleRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_PostRideSyncMultipleEndpoint'
  WHERE endpoint = 'RideAPI PostRideSyncMultipleEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpoint","param":"RideAPI TicketRideListEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_GetRideKaptureListEndpoint'
  WHERE endpoint = 'RideAPI TicketRideListEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_RideAPI_GetRideKaptureListEndpoint'
  WHERE endpoint = 'RideAPI GetRideKaptureListEndpoint';
