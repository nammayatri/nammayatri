-- {"api":"PostSelectEstimate","migration":"endpoint","param":"SelectAPI EstimatesEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_ESTIMATE'
  WHERE endpoint = 'SelectAPI EstimatesEndPoint';

-- {"api":"PostSelectCancelSearch","migration":"endpoint","param":"SelectAPI CancelSearchEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_CANCEL_SEARCH'
  WHERE endpoint = 'SelectAPI CancelSearchEndPoint';
