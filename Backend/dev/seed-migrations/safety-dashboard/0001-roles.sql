-- Bootstrap roles for atlas_safety_dashboard.
INSERT INTO atlas_safety_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('e5a69a26-d165-455a-a711-33a41e0d47c6', 'CUSTOMER',         'DASHBOARD_USER',  'customer',                                       '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('508a0bac-258d-44a6-ac55-aef57ab87a76', 'DRIVER',           'DASHBOARD_USER',  'driver',                                         '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('d5644e83-ffa3-4e0d-ae81-c3155eedb8fd', 'JUSPAY_OPS',       'DASHBOARD_USER',  'Juspay OPS',                                     '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('37947162-3b5d-4ed6-bcac-08841be1534d', 'JUSPAY_ADMIN',     'DASHBOARD_ADMIN', 'Juspay admin can create and assign other roles', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', 'CUSTOMER_SERVICE', 'DASHBOARD_USER',  'customer service',                               '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00')
ON CONFLICT (id) DO NOTHING;
