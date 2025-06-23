-- Add roles to the dashboard role table
INSERT INTO atlas_bap_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('e5a69a26-d165-455a-a711-33a41e0d4901', 'TICKET_DASHBOARD_USER', 'TICKET_DASHBOARD_USER', 'Ticket Dashboard User', now(), now()),
    ('e5a69a26-d165-455a-a711-33a41e0d4902', 'TICKET_DASHBOARD_MERCHANT', 'TICKET_DASHBOARD_MERCHANT', 'Ticket Dashboard Merchant', now(), now()),
    ('e5a69a26-d165-455a-a711-33a41e0d4903', 'TICKET_DASHBOARD_ADMIN', 'TICKET_DASHBOARD_ADMIN', 'Ticket Dashboard Admin', now(), now()),
    ('e5a69a26-d165-455a-a711-33a41e0d4904', 'TICKET_DASHBOARD_APPROVER', 'TICKET_DASHBOARD_APPROVER', 'Ticket Dashboard Approver', now(), now());
