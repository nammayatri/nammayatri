-- {"api":"GetFRFSTicketFrfsStageFareList","migration":"capability","param":"transit-operations.master.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_STAGE_FARE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PutFRFSTicketFrfsStageFareUpsert","migration":"capability","param":"transit-operations.master.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/PUT_FRFS_TICKET_FRFS_STAGE_FARE_UPSERT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetFRFSTicketFrfsTripCancelPreview","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_TRIP_CANCEL_PREVIEW' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketFrfsTripCancel","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_TRIP_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketFrfsTripCancelStatus","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_TRIP_CANCEL_STATUS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetFRFSTicketFrfsTripCancellations","migration":"capability","param":"transit-operations.trip.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.trip.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_TRIP_CANCELLATIONS' ) ON CONFLICT DO NOTHING;
