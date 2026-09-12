
-- {"api":"PostFrontendNotifyEvent","migration":"endpoint","param":"FlowStatusAPI NotifyEventEndPoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/FRONTEND/POST_FRONTEND_NOTIFY_EVENT'
  WHERE endpoint = 'FlowStatusAPI NotifyEventEndPoint';
