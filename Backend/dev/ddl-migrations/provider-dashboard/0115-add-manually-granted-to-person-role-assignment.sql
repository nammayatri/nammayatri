-- A city or merchant role an admin granted by hand, beside the person's own role. The assignment sync
-- leaves such a row alone while the person can still reach its city or merchant. Existing rows follow the
-- person's own role, so they default to false.
ALTER TABLE atlas_dashboard.person_role_assignment
  ADD COLUMN IF NOT EXISTS manually_granted boolean NOT NULL DEFAULT false;
