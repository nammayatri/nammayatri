-- Durable record of the merchant a person was provisioned under. Cross-merchant admin guards
-- read this instead of counting merchant_access rows, which any admin can delete.
-- Backfill for pre-existing rows lives in seed-migrations/rider-dashboard/0005-backfill-person-merchant-id.sql.
ALTER TABLE atlas_bap_dashboard.person
  ADD COLUMN IF NOT EXISTS merchant_id character(36) REFERENCES atlas_bap_dashboard.merchant (id);
