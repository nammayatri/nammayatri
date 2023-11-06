CREATE TABLE atlas_bpp_dashboard.merchant (
id character(36) NOT NULL,
short_id character varying(255) NOT NULL,
server_name character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  merchant_pkey PRIMARY KEY (id)
,CONSTRAINT  unique_short_id UNIQUE (short_id)
);
ALTER TABLE atlas_bpp_dashboard.merchant OWNER TO atlas_bpp_dashboard_user;

INSERT INTO atlas_bpp_dashboard.merchant (id, short_id, server_name, created_at) VALUES
    ('d92db186-39d3-48a4-ad1f-78a0c3f840fd', 'YATRI_PARTNER', 'DRIVER_OFFER_BPP', now ()),
    ('94bbea0d-3c52-479b-81f5-eca4969ae797', 'NAMMA_YATRI_PARTNER', 'DRIVER_OFFER_BPP', now ());

ALTER TABLE atlas_bpp_dashboard.server_access RENAME TO merchant_access;

ALTER TABLE atlas_bpp_dashboard.merchant_access
    ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_bpp_dashboard.merchant (id) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';

UPDATE atlas_bpp_dashboard.merchant_access
    SET merchant_id = '94bbea0d-3c52-479b-81f5-eca4969ae797' WHERE server_name = 'DRIVER_OFFER_BPP';

ALTER TABLE atlas_bpp_dashboard.merchant_access DROP COLUMN server_name;

ALTER TABLE atlas_bpp_dashboard.registration_token
    ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_bpp_dashboard.merchant (id) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';

UPDATE atlas_bpp_dashboard.registration_token
    SET merchant_id = '94bbea0d-3c52-479b-81f5-eca4969ae797' WHERE server_name = 'DRIVER_OFFER_BPP';

ALTER TABLE atlas_bpp_dashboard.registration_token DROP COLUMN server_name;
