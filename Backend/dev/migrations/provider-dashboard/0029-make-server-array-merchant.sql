INSERT INTO atlas_bpp_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('f6b70b37-d165-455a-a711-33a41e0d47c6', 'INTERNAL_ADMIN', 'DASHBOARD_ADMIN', 'InternalAdmin', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');

INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_names, created_at, is2fa_mandatory, default_operating_city, supported_operating_cities, server_name) VALUES
    ('e03ec297-39d3-48a4-ad1f-78a0c3f840fd', 'ADMINISTRATOR', '{DRIVER_OFFER_BPP, DRIVER_OFFER_BPP_MANAGEMENT}', '2022-09-12 15:15:42.104639+00', false, 'Bangalore', ARRAY['Bangalore'], 'DRIVER_OFFER_BPP');
