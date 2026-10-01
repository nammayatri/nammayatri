-- {"api":"PutFRFSTicketFrfsRouteTypeUpsert","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/PUT_FRFS_TICKET_FRFS_ROUTE_TYPE_UPSERT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketFrfsStageFareList","migration":"capability","param":"transit-operations.master.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_STAGE_FARE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PutFRFSTicketFrfsStageFareUpsert","migration":"capability","param":"transit-operations.master.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'transit-operations.master.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/PUT_FRFS_TICKET_FRFS_STAGE_FARE_UPSERT' ) ON CONFLICT DO NOTHING;
