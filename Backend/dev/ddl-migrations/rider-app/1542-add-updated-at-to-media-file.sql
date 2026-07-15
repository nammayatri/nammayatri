ALTER TABLE atlas_app.media_file ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
