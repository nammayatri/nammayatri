CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.end_otp_config ();

ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS trip_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS trip_mode text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS is_end_otp_required boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD COLUMN IF NOT EXISTS updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

-- Plain "ADD PRIMARY KEY" isn't idempotent (no IF NOT EXISTS variant in Postgres), so guard
-- it explicitly. Needed because local dev bootstraps by running migrations-read-only's full
-- schema reflection (which already creates this same table+PK) before ddl-migrations, so this
-- file can run against a DB that already has everything from CREATE TABLE onward.
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid = 'atlas_driver_offer_bpp.end_otp_config'::regclass AND contype = 'p'
  ) THEN
    ALTER TABLE atlas_driver_offer_bpp.end_otp_config ADD PRIMARY KEY (merchant_operating_city_id, trip_category, trip_mode);
  END IF;
END $$;

-- Seed data (one row per existing city per trip category/mode) lives in
-- dev/seed-migrations/dynamic-offer-driver-app/0001-end-otp-config-seed.sql — ddl-migrations
-- must be pure schema (enforced by the ddl-migrations-no-dml pre-commit hook).
