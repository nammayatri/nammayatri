ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN server_names Text[];

UPDATE atlas_bpp_dashboard.merchant as T1
SET server_names = ARRAY[server_name];

ALTER TABLE atlas_bpp_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;

UPDATE atlas_bpp_dashboard.merchant set server_names = '{DRIVER_OFFER_BPP, DRIVER_OFFER_BPP_MANAGEMENT}' where server_name = 'DRIVER_OFFER_BPP';