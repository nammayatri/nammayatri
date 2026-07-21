-- `is_bpp_sync_needed` is shared with provider-dashboard via the lib-dashboard `RoleT`
-- Beam type. It is INERT in rider-dashboard: never set, never read, no behavior change.
-- The column exists here only so the shared type's queries don't reference a missing
-- column. Nullable, no seed.

ALTER TABLE atlas_bap_dashboard.role
  ADD COLUMN is_bpp_sync_needed boolean;
