ALTER TABLE atlas_app.merchant ADD COLUMN public_media_file_url_pattern text DEFAULT 'https://dd17-106-51-81-97.ngrok-free.app/v2/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;
