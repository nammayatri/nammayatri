-- {"api":"GetFRFSTicketServiceCustomerFrfsConfig","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_CONFIG' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsAutocomplete","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_AUTOCOMPLETE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsRoutes","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsStations","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketServiceCustomerFrfsStationsPossibleStops","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS_POSSIBLE_STOPS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsRoute","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketServiceCustomerFrfsSearch","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsSearchQuote","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH_QUOTE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_QUOTE_V2_CONFIRM' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsBookingStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsRouteSeatLayout","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SEAT_LAYOUT' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostFRFSTicketServiceCustomerFrfsRouteServiceability","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SERVICEABILITY' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetFRFSTicketServiceCustomerFrfsTripRouteSeats","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES ( atlas_bap_dashboard.uuid_generate_v4(), '37947162-3b5d-4ed6-bcac-08841be1534d', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_TRIP_ROUTE_SEATS' ) ON CONFLICT DO NOTHING;
