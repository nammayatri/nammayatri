-- role values resolved against atlas_bap_dashboard.role rows seeded by
-- dev/seed-migrations/rider-dashboard/0001-roles.sql:
--   CUSTOMER         = e5a69a26-d165-455a-a711-33a41e0d47c6
--   DRIVER           = 508a0bac-258d-44a6-ac55-aef57ab87a76
--   JUSPAY_OPS       = d5644e83-ffa3-4e0d-ae81-c3155eedb8fd
--   JUSPAY_ADMIN     = 37947162-3b5d-4ed6-bcac-08841be1534d
--   CUSTOMER_SERVICE = a708c6a1-78b5-4e5e-9df8-468cd81dc2aa
INSERT INTO atlas_bap_dashboard.person (id, first_name, last_name, role_id, email_encrypted, email_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, password_hash, created_at, updated_at) VALUES
	('25e77f37-75e5-4665-8ed0-4be2af35940a', 'customer', 'customer', 'e5a69a26-d165-455a-a711-33a41e0d47c6', '0.1.0|0|/UkZKAXB/0WX+43O5fkXmKnuMzvJFUKwiI57hVKqpg9CMWCQ45wjXU/4JX/+I5xajr1FZvdc8WPPGbmFzOr7gknP63NC9wuXOw==', '\xe7aba02fcf4082bb36c9ea9f3b41e54de1307176ababd76e9059988f0aa57214', '0.1.0|0|j4VGXmuvt7HU5qP+En30NqGAn2z8KpRGFeJVd8H/VAWPB3mliuNjeQWteRoXUC7yYOOMjV6m0uYkJqy/Ww==', '\x1604f4cfb781d8f1c9ff024f8d06abc265a2d6954ac59079ea0c11f9f0bc1fa4', '+91', '\xea29f9013ed2a5b6da7f20b3c3b3f429bd8f55413dc35fc98403d017edc6cd1d', '2022-09-06 11:25:45.564321+00', '2022-09-06 11:25:45.564321+00'),
	('cd69ae25-1641-4e6d-b9f4-5bae63e2a537', 'driver', 'driver', '508a0bac-258d-44a6-ac55-aef57ab87a76', '0.1.0|2|RW7L3VAgJptc2fyG6mEn1P6rUxYagnT9AC5bhnKBmBkEVlR9IhsXLOUuu0F4SvUWozkQYouvMgARNL6x/tlVRm/YA6udkmU=', '\x7e25a5f3b66b86b00d1e2c7c3aacc9de05f5b64ca6e953bd825d861fa0206ee2', '0.1.0|0|O4pIfqluEqP2UWbMcSDj26BCt5eKapJDHhvFajnrwFKYC7ux8MmPXLUqEkA3pfHz5dOu4LgF/qrHdMklKA==', '\x017cc744055f27a166b08a99b08adff09d77cd2fd36f9017b17c749bbaff3fb8', '+91', '\x1b2c79ff2ba0b4eacb1fff0944b89be973f77c43a989abcc3fbf2762003d1cf9', '2022-09-06 11:25:44.591696+00', '2022-09-06 11:25:44.591696+00'),
	('a77b0507-ae01-42dd-a075-264f59d89049', 'juspay_ops', 'juspay_ops', 'd5644e83-ffa3-4e0d-ae81-c3155eedb8fd', '0.1.0|1|2Yjtb/THzo0gn3Nrlv/nUD5loCHgZ+sF8qdo1tLZpmzmUNsTLDKTbW6jDlWCBAl/U3vzYWaOZRTU7QULoq9Rn6bgOaSqiM7Eun2I', '\x2a796a562e683a3dca90cb8e17fe5781ef59d31b318379c292e40314e480f3cd', '0.1.0|0|6UvxFEKkKoAcTNBTUIsioH1eS7oqmlf8M6D21qTqyzfUtZBdx9pLgu6Id0unL08M4zVWYVyJXby//SGtRQ==', '\xd9a0478bcb0f95155df43d639bb5d368118d3d74f804e7c16f89679abf274c14', '+91', '\x1610d494c1be63c14a1dfb6a5797c5e16dcbfcc836d9c073863ed4e27ee4c4bb', '2022-09-06 11:25:43.601821+00', '2022-09-06 11:25:43.601821+00'),
	('3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'juspay_admin', 'juspay_admin', '37947162-3b5d-4ed6-bcac-08841be1534d', '0.1.0|0|LhbMPLXsyXE0tjkVpk2AsylStET+zn3gLufYYvF+mWEGaXojqY71IUsw/gJWIIWzbQTGsY31FlnT3BL8o360B2kngyHgMg9A3Jnj0I4=', '\xef2654345b65cbe5230f3cc47ff26ff73cfd7023e10ac258b4b88bab8221a181', '0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==', '\x26d21f3ddcce96b1fab220d6aea0b5341d4653e812d4e18d542577acbdeef640', '+91',	'\x8c03a02fbcb46d7f7624063574892f64f19b9871138edfcfeb4f0361362e567f', '2022-09-06 11:25:42.609155+00', '2022-09-06 11:25:42.609155+00'),
	('59f5bd8c-8268-4e8e-bcab-c21da7e496d4', 'customer_service', 'customer_service', 'a708c6a1-78b5-4e5e-9df8-468cd81dc2aa', '0.1.0|1|9KSgxavNrMrth8w2zBzxhW0dA+yTK+Pub7awBH//7Fzu8fnZn3OWWGW0sLu6AKHtg9DdAD8bVG9r0n10P5d/507Aq8A6M7bEwMMzTJwCuXwB', '\x665ddab8860069681b2a5727b8983315408a39a6f3064734bd146236bd825438', '0.1.0|2|JaxIBt2gn9eHBYnoDpSF3hJm+KtfXNHtzaR9LZVEZUHnM6lxscEfLdGMj+5e9suZwVA0sMlKFUhr/7JmMw==', '\xbb63874220d75c23f6b49a63e2c0232408d0dc49c0e1c1bd3c004400488f0db5', '+91', '\x7aa08a0e89a92bd13ce544fa7bd58fbada3862919306a072240df98470e42792', '2022-09-06 11:25:41.633041+00', '2022-09-06 11:25:41.633041+00');

INSERT INTO atlas_bap_dashboard.registration_token (id, token, person_id, operating_city, created_at) VALUES
	('50666614-c6f1-48a8-ab16-23873b93f492', '512e0a7b-e521-4d6b-a0c4-8713ae345bf2', '25e77f37-75e5-4665-8ed0-4be2af35940a', 'Bangalore', now ()),
	('57b6a4ed-6f74-4260-93eb-5c985fd5d776', '223ed551-c9f8-4bae-b49a-30a696424b7f', 'cd69ae25-1641-4e6d-b9f4-5bae63e2a537', 'Bangalore', now ()),
	('a7f18c42-43bf-4ab9-9fdd-846efa83f2dc', 'e789b47f-2e39-47a2-b297-33f8fd4b87d0', 'a77b0507-ae01-42dd-a075-264f59d89049', 'Bangalore', now ()),
	('b856907d-9fb3-4804-9ae4-a53ca902ea0d', '0f3378e2-da5b-4eac-a0f6-397ca48358de', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', 'Bangalore', now ()),
	('c9c0375f-5885-4dab-acc9-27552c31db90', '07ef2a2a-b014-4a86-b1c0-e453e8b0b66e', '59f5bd8c-8268-4e8e-bcab-c21da7e496d4', 'Bangalore', now ());

-- ── Admin merchant_access for the test admin person, per merchant × city ──
-- atlas_bap_dashboard.merchant_operating_city / merchant come from config-sync.
-- We only seed merchant_access rows so the local test admin can switch
-- merchants/cities in the dashboard.

DO $$
DECLARE
    admin_person_id TEXT := '3680f4b5-dce4-4d03-aa8c-5405690e87bd';
    r RECORD;
    city_name TEXT;
BEGIN
    FOR r IN SELECT id, short_id FROM atlas_bap_dashboard.merchant
    LOOP
        FOREACH city_name IN ARRAY ARRAY['Bangalore','Helsinki','Kolkata','Chennai','Delhi']
        LOOP
            INSERT INTO atlas_bap_dashboard.merchant_access (id, person_id, merchant_id, merchant_short_id, operating_city, secret_key, is2fa_enabled, created_at)
            SELECT gen_random_uuid()::text, admin_person_id, r.id, r.short_id, city_name, '', false, now()
            WHERE NOT EXISTS (
                SELECT 1 FROM atlas_bap_dashboard.merchant_access
                WHERE person_id = admin_person_id AND operating_city = city_name AND merchant_id = r.id
            );
        END LOOP;
    END LOOP;

    INSERT INTO atlas_bap_dashboard.registration_token (id, token, person_id, created_at, merchant_id, operating_city, enabled)
    SELECT gen_random_uuid()::text, 'bap-admin-token-for-testing', admin_person_id, now(),
           (SELECT id FROM atlas_bap_dashboard.merchant LIMIT 1), 'Bangalore', true
    WHERE NOT EXISTS (
        SELECT 1 FROM atlas_bap_dashboard.registration_token WHERE token = 'bap-admin-token-for-testing'
    );
END $$;
-- ── Restore DEFAULT + NOT NULL on FK columns (originally inlined in DDL ADD COLUMN) ─────
-- Backfill existing NULL rows with the default UUID (the same DEFAULT the original migration used),
-- then re-apply the DEFAULT and NOT NULL constraints stripped from the migrations.

UPDATE atlas_bap_dashboard.person
   SET role_id = 'e5a69a26-d165-455a-a711-33a41e0d47c6'
 WHERE role_id IS NULL;
ALTER TABLE atlas_bap_dashboard.person
   ALTER COLUMN role_id SET DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6';
ALTER TABLE atlas_bap_dashboard.person
   ALTER COLUMN role_id SET NOT NULL;

UPDATE atlas_bap_dashboard.merchant_access
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bap_dashboard.merchant_access
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bap_dashboard.merchant_access
   ALTER COLUMN merchant_id SET NOT NULL;

UPDATE atlas_bap_dashboard.registration_token
   SET merchant_id = 'd92db186-39d3-48a4-ad1f-78a0c3f840fd'
 WHERE merchant_id IS NULL;
ALTER TABLE atlas_bap_dashboard.registration_token
   ALTER COLUMN merchant_id SET DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';
ALTER TABLE atlas_bap_dashboard.registration_token
   ALTER COLUMN merchant_id SET NOT NULL;
