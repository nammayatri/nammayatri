INSERT INTO atlas_bpp_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'juspay_admin', 'juspay_admin', '37947162-3b5d-4ed6-bcac-08841be1534d', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00');

-- ── Admin merchant_access for the test admin person, per merchant × city ──
-- atlas_bpp_dashboard.merchant_operating_city / merchant come from config-sync.
-- We only seed merchant_access rows so the local test admin can switch
-- merchants/cities in the dashboard.

DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
    r RECORD;
    city_name TEXT;
BEGIN
    FOR r IN SELECT id, short_id FROM atlas_bpp_dashboard.merchant
    LOOP
        FOREACH city_name IN ARRAY ARRAY['Bangalore','Helsinki','Kolkata','Chennai','Delhi']
        LOOP
            INSERT INTO atlas_bpp_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
            SELECT gen_random_uuid()::text, admin_person_id, r.id, r.short_id, city_name, '', false, now()
            WHERE NOT EXISTS (
                SELECT 1 FROM atlas_bpp_dashboard.merchant_access
                WHERE person_id = admin_person_id AND operating_city = city_name AND merchant_id = r.id
            );
        END LOOP;
    END LOOP;
END $$;

-- ── from 0077-registration-token-dashboard.sql ─────────────────────────────────
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
-- ── Restore DEFAULT + NOT NULL on FK columns (originally inlined in DDL ADD COLUMN) ─────
UPDATE atlas_bpp_dashboard.person
   SET role_id = 'e5a69a26-d165-455a-a711-33a41e0d47c6'
 WHERE role_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.person
   ALTER COLUMN role_id SET DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6';
ALTER TABLE atlas_bpp_dashboard.person
   ALTER COLUMN role_id SET NOT NULL;

UPDATE atlas_bpp_dashboard.merchant_access
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.merchant_access
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bpp_dashboard.merchant_access
   ALTER COLUMN merchant_id SET NOT NULL;

UPDATE atlas_bpp_dashboard.registration_token
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bpp_dashboard.registration_token
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bpp_dashboard.registration_token
   ALTER COLUMN merchant_id SET NOT NULL;
