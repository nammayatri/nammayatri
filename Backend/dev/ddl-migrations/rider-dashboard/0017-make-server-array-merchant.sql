ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN server_names Text[];

ALTER TABLE atlas_bap_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;