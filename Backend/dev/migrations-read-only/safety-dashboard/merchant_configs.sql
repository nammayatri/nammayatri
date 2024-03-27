CREATE TABLE atlas_safety_dashboard.merchant_configs ();

ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN merchant_short_id text NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN request_web_hook boolean NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN web_hook_headers text[] NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN web_hook_url text NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.merchant_configs ADD PRIMARY KEY ( id);