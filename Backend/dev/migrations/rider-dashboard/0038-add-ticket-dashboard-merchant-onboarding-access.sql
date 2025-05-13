-- Add access matrix entries for ticket dashboard merchant onboarding actions
-- For TICKET_DASHBOARD_USER role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE', now(), now());

-- For TICKET_DASHBOARD_MERCHANT role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE', now(), now());

-- For TICKET_DASHBOARD_ADMIN role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOADING_LIST_ALL', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE', now(), now());

-- For TICKET_DASHBOARD_APPROVER role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOADING_LIST_ALL', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4904', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE', now(), now());



------------------------- EXTRA PERMISSIONS -------------------------


-- For TICKET_DASHBOARD_MERCHANT role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACES', now(), now());

-- For TICKET_DASHBOARD_ADMIN role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACES', now(), now());

-- ASSET MANAGEMENT

-- For TICKET_DASHBOARD_MERCHANT role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_UPLOAD_ASSET', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_DELETE_ASSET', now(), now());

-- For TICKET_DASHBOARD_ADMIN role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_UPLOAD_ASSET', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_DELETE_ASSET', now(), now());

-- Cancellation
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4901', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL', now(), now());



-- seat management
-- For TICKET_DASHBOARD_MERCHANT role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_CURRENT_SEAT_STATUS', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4902', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_SEAT_MANAGEMENT', now(), now());

-- For TICKET_DASHBOARD_ADMIN role
INSERT INTO atlas_bap_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at) VALUES
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_CURRENT_SEAT_STATUS', now(), now()),
    (atlas_bap_dashboard.uuid_generate_v4(), 'e5a69a26-d165-455a-a711-33a41e0d4903', 'DSL', 'USER_FULL_ACCESS', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_SEAT_MANAGEMENT', now(), now());