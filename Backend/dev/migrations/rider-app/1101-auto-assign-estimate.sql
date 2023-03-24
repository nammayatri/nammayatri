ALTER TABLE atlas_app.estimate
ADD COLUMN auto_assign_enabled boolean NOT NULL DEFAULT FALSE;

ALTER TABLE atlas_app.estimate
ADD COLUMN auto_assign_enabled_v2 boolean NOT NULL DEFAULT FALSE;

ALTER TABLE atlas_app.estimate
ADD COLUMN auto_assign_quote_id text;