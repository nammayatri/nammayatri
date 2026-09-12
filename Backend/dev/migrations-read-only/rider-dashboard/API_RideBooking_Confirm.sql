-- {"api":"PostConfirmRideSearchQuotes","migration":"endpoint","param":"ConfirmAPI ConfirmEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES'
  WHERE endpoint = 'ConfirmAPI ConfirmEndPoint';
