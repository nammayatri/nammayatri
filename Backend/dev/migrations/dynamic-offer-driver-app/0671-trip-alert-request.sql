-- Note: This Would Be Run During The Hot Push, Do Not Run Again During Main Release
INSERT INTO
  atlas_driver_offer_bpp.trip_alert_request (
    alert_request_id,
    alert_request_type,
    driver_id,
    fleet_owner_id,
    id,
    merchant_id,
    merchant_operating_city_id,
    route_code,
    trip_transaction_id,
    created_at,
    updated_at
  )
SELECT
  end_ride_approval_request_id,
  'EndRideApproval',
  driver_id,
  fleet_owner_id,
  md5(random() :: text || clock_timestamp() :: text) :: uuid,
  merchant_id,
  merchant_operating_city_id,
  route_code,
  id,
  created_at,
  updated_at
FROM
  atlas_driver_offer_bpp.trip_transaction
WHERE
  end_ride_approval_request_id IS NOT NULL;