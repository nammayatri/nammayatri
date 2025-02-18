ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN email_encrypted DROP NOT NULL;
ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN email_hash DROP NOT NULL;
ALTER TABLE atlas_bpp_dashboard.person ALTER COLUMN password_hash  DROP NOT NULL;