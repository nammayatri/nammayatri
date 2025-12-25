-- QUERIES ONLY FOR LOCAL
ALTER TABLE atlas_app.media_file ADD COLUMN s3_file_path text;
UPDATE atlas_app.rider_config SET dashboard_media_file_url_pattern = 'http://localhost:8017/bap/NAMMA_YATRI/media/file?filePath=<FILE_PATH>';