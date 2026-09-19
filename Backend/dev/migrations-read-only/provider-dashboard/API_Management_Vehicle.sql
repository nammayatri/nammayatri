-- {"api":"PostVehicleParkingFeeExemption","migration":"capability","param":"city-operations.onboarding.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE/POST_VEHICLE_PARKING_FEE_EXEMPTION' ) ON CONFLICT DO NOTHING;
