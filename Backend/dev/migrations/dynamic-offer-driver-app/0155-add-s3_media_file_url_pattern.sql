ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN s3_media_file_url_pattern Text NOT NULL DEFAULT '/<DOMAIN>/s3media?filePath=<FILE_PATH>';


