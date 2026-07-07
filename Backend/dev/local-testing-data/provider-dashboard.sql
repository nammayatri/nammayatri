-- unencrypted: email: juspay_admin@dashboard.com, password: juspay_admin
INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'juspay_admin', 'juspay_admin', '37947162-3b5d-4ed6-bcac-08841be1534d', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00')
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
SELECT
    'local-admin-token-blr-id-00000000000',
    'local-admin-token-bangalore-namma-yatri',
    '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
    m.id,
    'Bangalore',
    true,
    now()
FROM atlas_bpp_dashboard.merchant m
WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
ON CONFLICT DO NOTHING;

-- unencrypted: email: fleet@dashboard.com, password: fleet
INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('f1eef1ee-f1ee-f1ee-f1ee-f1eef1eef1ee', 'fleet', 'admin', 'e5a69a26-d165-455a-a711-33a41e0d4812', '0.1.0|0|FmlWDEUp8Ya8cKLaAFVOcynY2DOa+evTh51LvGcot0vVuFSDcG4NfiIUH8rGDi3ZS8BWIl83heEUhatu3d8yXXmc0+ARWQ==', '\x4d383194f7abb5422eed9d10feb4b30ac2ba06f88ff0e0aa35262d487db86d4a', '0.1.0|1|pxBx5cEmeWjlvXC9tbqoZLzcHMyQ4uQuVMXenbfVljWalGKeBsJKzcu8XFbi3BvdxX/moxwToQ7IhN9owA==', '\x0e0369d00c4209c0bdffeb830cbee1c56d57b3ad2794c5616c585e650de2653a', '+91', '\xec75c6cc9bee0b826ae77393d7e91558cc71ac943b7b37656bf057172eb791c5', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00')
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bpp_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
SELECT
    'local-fleet-token-blr-id-00000000000',
    'local-fleet-token-bangalore-namma-yatri',
    'f1eef1ee-f1ee-f1ee-f1ee-f1eef1eef1ee',
    m.id,
    'Bangalore',
    true,
    now()
FROM atlas_bpp_dashboard.merchant m
WHERE m.short_id = 'NAMMA_YATRI_PARTNER'
ON CONFLICT DO NOTHING;

-- Grant the FLEET dev token cross-merchant + cross-city dashboard access.
-- (2FA columns moved from merchant_access to person after the refactor; access
-- rows carry no per-merchant 2FA state anymore.)
DO $$
DECLARE
    fleet_person_id TEXT := 'f1eef1ee-f1ee-f1ee-f1ee-f1eef1eef1ee';
BEGIN
    INSERT INTO atlas_bpp_dashboard.merchant_access
        (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
    SELECT gen_random_uuid()::text,
           fleet_person_id,
           m.id,
           m.short_id,
           c.city,
           now()
    FROM atlas_bpp_dashboard.merchant m
    CROSS JOIN LATERAL unnest(m.supported_operating_cities) AS c(city)
    WHERE NOT EXISTS (
        SELECT 1 FROM atlas_bpp_dashboard.merchant_access ma
        WHERE ma.person_id   = fleet_person_id
          AND ma.merchant_id = m.id
          AND ma.operating_city::text = c.city
    );
END $$;

-- Fleet owner + operator roles: grant access to dashboard analytics and vehicle list stats endpoints
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type, created_at, updated_at)
SELECT gen_random_uuid(), r.id, 'DSL', 'USER_FULL_ACCESS', perm.action, now(), now()
FROM atlas_bpp_dashboard.role r
CROSS JOIN (VALUES
  ('PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS'),
  ('PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS_ALL_TIME'),
  ('PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_LIST_STATS'),
  ('PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DASHBOARD_ANALYTICS_CACHE'),
  ('PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS'),
  ('PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS_ALL_TIME')
) AS perm(action)
WHERE r.dashboard_access_type IN ('FLEET_OWNER', 'DASHBOARD_OPERATOR')
ON CONFLICT DO NOTHING;

-- Grant the JUSPAY_ADMIN dev token cross-merchant + cross-city dashboard access.
DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
BEGIN
    INSERT INTO atlas_bpp_dashboard.merchant_access
        (id, person_id, merchant_id, merchant_short_id, operating_city, created_at)
    SELECT gen_random_uuid()::text,
           admin_person_id,
           m.id,
           m.short_id,
           c.city,
           now()
    FROM atlas_bpp_dashboard.merchant m
    CROSS JOIN LATERAL unnest(m.supported_operating_cities) AS c(city)
    WHERE NOT EXISTS (
        SELECT 1 FROM atlas_bpp_dashboard.merchant_access ma
        WHERE ma.person_id   = admin_person_id
          AND ma.merchant_id = m.id
          AND ma.operating_city::text = c.city
    );
END $$;