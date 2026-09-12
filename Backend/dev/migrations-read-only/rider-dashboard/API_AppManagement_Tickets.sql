-- {"api":"PostTicketsVerify","migration":"endpoint","param":"TicketsAPI VerifyBookingDetails","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_VERIFY'
  WHERE endpoint = 'TicketsAPI VerifyBookingDetails';
