-- {"api":"PostBookingCancelAllStuck","migration":"endpoint","param":"BookingAPI StuckBookingsCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_BookingAPI_PostBookingCancelAllStuckEndpoint'
  WHERE endpoint = 'BookingAPI StuckBookingsCancelEndpoint';

-- {"api":"PostBookingCancelAllStuck","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_BookingAPI_PostBookingCancelAllStuckEndpoint'
  WHERE endpoint = 'BookingAPI PostBookingCancelAllStuckEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpoint","param":"BookingAPI MultipleBookingSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_BookingAPI_PostBookingSyncMultipleEndpoint'
  WHERE endpoint = 'BookingAPI MultipleBookingSyncEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_BookingAPI_PostBookingSyncMultipleEndpoint'
  WHERE endpoint = 'BookingAPI PostBookingSyncMultipleEndpoint';
