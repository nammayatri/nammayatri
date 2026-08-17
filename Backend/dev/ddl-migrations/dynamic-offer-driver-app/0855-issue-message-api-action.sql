-- Mirrors the rider-app 1555 migration: issue_message is a shared
-- IssueManagement table, so the column must exist in this schema too.
-- IF NOT EXISTS: some dev environments received this column by hand before
-- the file landed.
ALTER TABLE atlas_driver_offer_bpp.issue_message
  ADD COLUMN IF NOT EXISTS api_action text;
