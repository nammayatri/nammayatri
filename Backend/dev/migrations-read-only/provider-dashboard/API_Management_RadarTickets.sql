-- {"api":"GetRadarTicketsList","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRadarTicketsSummary","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_SUMMARY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRadarTicketsReporters","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_REPORTERS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRadarTicketsTicket","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRadarTicketsTicketAttachment","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET_ATTACHMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRadarTicketsTicketReply","migration":"capability","param":"support.radar.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_REPLY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetRadarTicketsTicketConversation","migration":"capability","param":"support.radar.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/GET_RADAR_TICKETS_TICKET_CONVERSATION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRadarTicketsCreate","migration":"capability","param":"support.radar.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_CREATE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRadarTicketsTicketUpdateStatus","migration":"capability","param":"support.radar.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_UPDATE_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRadarTicketsTicketCsat","migration":"capability","param":"support.radar.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'support.radar.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RADAR_TICKETS/POST_RADAR_TICKETS_TICKET_CSAT' ) ON CONFLICT DO NOTHING;
