-- Optional free-text reason captured when an admin deletes a user.
ALTER TABLE atlas_bap_dashboard.deleted_user ADD COLUMN IF NOT EXISTS delete_reason text;
