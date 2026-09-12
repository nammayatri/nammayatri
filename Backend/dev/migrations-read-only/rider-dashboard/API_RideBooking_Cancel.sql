-- {"api":"PostCancelBooking","migration":"endpoint","param":"CancelAPI RideBookingCancelEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/CANCEL/POST_CANCEL_BOOKING'
  WHERE endpoint = 'CancelAPI RideBookingCancelEndPoint';
