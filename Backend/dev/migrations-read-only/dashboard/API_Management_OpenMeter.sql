-- {"api":"GetOpenMeterRideList","migration":"capability","param":"city-operations.ride.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/OPEN_METER/GET_OPEN_METER_RIDE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetOpenMeterRideInfo","migration":"capability","param":"city-operations.ride.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/OPEN_METER/GET_OPEN_METER_RIDE_INFO' ) ON CONFLICT DO NOTHING;
