-- {"api":"PostFRFSTicketServiceCustomerFrfsBookingCancel","migration":"capability","param":"city-operations.frfs.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.frfs.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketServiceCustomerFrfsBookingReschedule","migration":"capability","param":"city-operations.frfs.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.frfs.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_RESCHEDULE' ) ON CONFLICT DO NOTHING;
