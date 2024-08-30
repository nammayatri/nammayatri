-- {"api":"PostBookingCancelAllStuck","migration":"endpoint","param":"BookingAPI StuckBookingsCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'
  WHERE endpoint = 'BookingAPI StuckBookingsCancelEndpoint';

-- {"api":"PostBookingCancelAllStuck","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'
  WHERE endpoint = 'BookingAPI PostBookingCancelAllStuckEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpoint","param":"BookingAPI MultipleBookingSyncEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'
  WHERE endpoint = 'BookingAPI MultipleBookingSyncEndpoint';

-- {"api":"PostBookingSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'
  WHERE endpoint = 'BookingAPI PostBookingSyncMultipleEndpoint';
