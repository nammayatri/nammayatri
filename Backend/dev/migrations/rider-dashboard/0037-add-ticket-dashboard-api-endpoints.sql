-- Add access matrix entries for ticket dashboard user info endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO', now(), now());

-- Add access matrix entries for ticket dashboard file endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE', now(), now());

-- Add access matrix entries for ticket dashboard login auth endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH', now(), now());

-- Add access matrix entries for ticket dashboard login verify endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY', now(), now());

-- Add access matrix entries for ticket dashboard register endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_REGISTER', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_REGISTER', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_REGISTER', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_REGISTER', now(), now());

-- Add access matrix entries for ticket dashboard agreement endpoint
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT', now(), now());
