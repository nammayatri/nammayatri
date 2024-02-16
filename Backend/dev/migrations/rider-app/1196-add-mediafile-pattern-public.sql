ALTER TABLE atlas_app.merchant ADD COLUMN public_media_file_url_pattern text DEFAULT 'http://localhost:8013/v2/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;
