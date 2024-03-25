ALTER TABLE atlas_app.merchant ADD COLUMN public_media_file_url_pattern text DEFAULT 'https://0fc0-65-1-52-128.ngrok-free.app/v2/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;
