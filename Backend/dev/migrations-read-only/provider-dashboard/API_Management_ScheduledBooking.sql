-- {"api":"GetScheduledBookingList","migration":"capability","param":"city-operations.scheduled-bookings.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetScheduledBookingInfo","migration":"capability","param":"city-operations.scheduled-bookings.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetScheduledBookingDriverDistance","migration":"capability","param":"city-operations.scheduled-bookings.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_DRIVER_DISTANCE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostScheduledBookingAssign","migration":"capability","param":"city-operations.scheduled-bookings.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_ASSIGN' ) ON CONFLICT DO NOTHING;

-- {"api":"PostScheduledBookingUnassign","migration":"capability","param":"city-operations.scheduled-bookings.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_UNASSIGN' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetScheduledBookingNearbyDrivers","migration":"capability","param":"city-operations.scheduled-bookings.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/GET_SCHEDULED_BOOKING_NEARBY_DRIVERS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostScheduledBookingOpsNote","migration":"capability","param":"city-operations.scheduled-bookings.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SCHEDULED_BOOKING/POST_SCHEDULED_BOOKING_OPS_NOTE' ) ON CONFLICT DO NOTHING;
