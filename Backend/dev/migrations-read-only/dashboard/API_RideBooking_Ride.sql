-- {"api":"PostRideStart","migration":"endpoint","param":"RideAPI RideStartEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_START'
  WHERE endpoint = 'RideAPI RideStartEndpoint';

-- {"api":"PostRideEnd","migration":"endpoint","param":"RideAPI RideEndEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_END'
  WHERE endpoint = 'RideAPI RideEndEndpoint';

-- {"api":"PostRideCancel","migration":"endpoint","param":"RideAPI RideCancelEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_CANCEL'
  WHERE endpoint = 'RideAPI RideCancelEndpoint';

-- {"api":"PostRideBookingWithVehicleNumberAndPhone","migration":"endpoint","param":"RideAPI BookingWithVehicleNumberAndPhoneEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE'
  WHERE endpoint = 'RideAPI BookingWithVehicleNumberAndPhoneEndpoint';
