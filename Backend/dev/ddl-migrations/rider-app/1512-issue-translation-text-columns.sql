------------------------------------------------------------------------------------------------
-- NOTE: This is to fix local schema to make it similar as master schema for config-sync import.
-- Do not run in master or prod
------------------------------------------------------------------------------------------------

ALTER TABLE atlas_app.issue_translation ALTER COLUMN sentence TYPE text;
ALTER TABLE atlas_app.issue_translation ALTER COLUMN translation TYPE text;
