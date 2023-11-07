ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN server_names Text[];

UPDATE atlas_bap_dashboard.merchant as T1
SET server_names = ARRAY[server_name];

ALTER TABLE atlas_bap_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;

UPDATE atlas_bap_dashboard.merchant set server_names = '{APP_BACKEND, APP_BACKEND_MANAGEMENT}' where server_name = 'APP_BACKEND';