-- unencrypted: email: juspay_admin@dashboard.com, password: juspay_admin
INSERT INTO atlas_bap_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'juspay_admin', 'juspay_admin', '37947162-3b5d-4ed6-bcac-08841be1534d', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00')
ON CONFLICT DO NOTHING;

INSERT INTO atlas_bap_dashboard.registration_token (id, token, person_id, merchant_id, operating_city, enabled, created_at)
SELECT
    'local-admin-token-blr-id-00000000000',
    'local-admin-token-bangalore-namma-yatri',
    '3680f4b5-dce4-4d03-aa8c-5405690e87bd',
    m.id,
    'Bangalore',
    true,
    now()
FROM atlas_bap_dashboard.merchant m
WHERE m.short_id = 'NAMMA_YATRI'
ON CONFLICT DO NOTHING;

-- Grant the JUSPAY_ADMIN dev token cross-merchant + cross-city dashboard access
-- on the rider-dashboard side. Same pattern as provider-dashboard.sql — cities
-- come from each merchant's `supported_operating_cities` array. Idempotent.
DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
BEGIN
    INSERT INTO atlas_bap_dashboard.merchant_access
        (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
    SELECT gen_random_uuid()::text,
           admin_person_id,
           m.id,
           m.short_id,
           c.city,
           '',
           false,
           now()
    FROM atlas_bap_dashboard.merchant m
    CROSS JOIN LATERAL unnest(m.supported_operating_cities) AS c(city)
    WHERE NOT EXISTS (
        SELECT 1 FROM atlas_bap_dashboard.merchant_access ma
        WHERE ma.person_id   = admin_person_id
          AND ma.merchant_id = m.id
          AND ma.operating_city::text = c.city
    );
END $$;