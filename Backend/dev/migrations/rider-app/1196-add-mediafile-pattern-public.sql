ALTER TABLE atlas_app.merchant ADD COLUMN public_media_file_url_pattern text DEFAULT 'https://f57b-13-232-74-226.ngrok-free.app/v2/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;
