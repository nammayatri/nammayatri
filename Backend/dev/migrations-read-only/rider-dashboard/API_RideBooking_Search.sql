-- {"api":"PostSearchRide","migration":"endpoint","param":"SearchAPI SearchEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/SEARCH/POST_SEARCH_RIDE'
  WHERE endpoint = 'SearchAPI SearchEndPoint';
