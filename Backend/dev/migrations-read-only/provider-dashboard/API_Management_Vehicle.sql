-- {"api":"GetVehicleInfo","migration":"capability","param":"city-operations.vehicle.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE/GET_VEHICLE_INFO' ) ON CONFLICT DO NOTHING;
