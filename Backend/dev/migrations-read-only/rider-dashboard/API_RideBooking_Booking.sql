-- {"api":"PostBookingStatus","migration":"endpoint","param":"RBooking RideStatusEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS'
  WHERE endpoint = 'RBooking RideStatusEndPoint';
