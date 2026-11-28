-- {"api":"GetBookingSearch","migration":"capability","param":"city-operations.booth_booking.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_SEARCH' ) ON CONFLICT DO NOTHING;
