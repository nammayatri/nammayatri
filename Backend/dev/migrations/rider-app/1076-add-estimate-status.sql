ALTER TABLE atlas_app.estimate ADD COLUMN status character varying(255);
ALTER TABLE atlas_app.estimate ADD COLUMN updated_at timestamp with time zone NOT NULL;