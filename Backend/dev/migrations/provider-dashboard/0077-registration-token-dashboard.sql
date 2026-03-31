-- Admin token for Bangalore with correct merchant_id and operating_city
INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
VALUES (
    'local-admin-token-blr-id-00000000000',
    'local-admin-token-bangalore-namma-yatri',
    '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
    '94bbea0d-3c52-479b-81f5-eca4969ae797',
    'Bangalore',
    true,
    now()
);
