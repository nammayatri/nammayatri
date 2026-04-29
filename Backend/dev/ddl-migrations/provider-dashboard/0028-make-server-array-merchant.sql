ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN server_names Text[];

ALTER TABLE atlas_bpp_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;