ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN email_encrypted DROP NOT NULL;
ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN email_hash DROP NOT NULL;
ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN password_hash  DROP NOT NULL;
INSERT INTO atlas_bpp_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('e5a69a26-d165-455a-a711-33a41e0d4812', 'FLEET', 'FLEET_OWNER', 'fleetOwner', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');