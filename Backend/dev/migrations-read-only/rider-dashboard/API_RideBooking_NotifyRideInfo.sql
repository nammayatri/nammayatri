-- {"api":"PostNotifyRideInfoNotifyRideInfo","migration":"endpoint","param":"NotifyRideInfo NotifyRideInfoEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_RIDE_BOOKING/NOTIFY_RIDE_INFO/POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO'
  WHERE endpoint = 'NotifyRideInfo NotifyRideInfoEndpoint';
