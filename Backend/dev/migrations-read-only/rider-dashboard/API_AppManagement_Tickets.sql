-- {"api":"PostTicketsVerify","migration":"endpoint","param":"TicketsAPI VerifyBookingDetails","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_VERIFY'
  WHERE endpoint = 'TicketsAPI VerifyBookingDetails';

-- {"api":"PostTicketsVerify","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS VERIFY_BOOKING_DETAILS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_VERIFY' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'VERIFY_BOOKING_DETAILS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTicketsServices","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS GET_TICKET_SERVICES","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICES' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'GET_TICKET_SERVICES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTicketsPlaces","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS GET_TICKET_PLACES","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_PLACES' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'GET_TICKET_PLACES' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTicketsUpdate","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS UPDATE_SEAT_MANAGEMENT","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_UPDATE' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'UPDATE_SEAT_MANAGEMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTicketsBookingsCancel","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS CANCEL_TICKET_BOOKING","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_BOOKINGS_CANCEL' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'CANCEL_TICKET_BOOKING' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTicketsServiceCancel","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT MERCHANT CANCEL_TICKET_SERVICE","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICE_CANCEL' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MERCHANT' AND T1.user_action_type = 'CANCEL_TICKET_SERVICE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTicketsBookingDetails","migration":"userActionType","param":"ApiAuth APP_BACKEND_MANAGEMENT CUSTOMERS GET_TICKET_BOOKING_DETAILS","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bap_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_BOOKING_DETAILS' FROM atlas_bap_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'CUSTOMERS' AND T1.user_action_type = 'GET_TICKET_BOOKING_DETAILS' ) ON CONFLICT DO NOTHING;
