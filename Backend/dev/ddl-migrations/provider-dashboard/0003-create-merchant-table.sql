CREATE TABLE atlas_bpp_dashboard.merchant (
id character(36) NOT NULL,
short_id character varying(255) NOT NULL,
server_name character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  merchant_pkey PRIMARY KEY (id)
,CONSTRAINT  unique_short_id UNIQUE (short_id)
);
ALTER TABLE atlas_bpp_dashboard.merchant OWNER TO atlas_bpp_dashboard_user;

ALTER TABLE atlas_bpp_dashboard.server_access RENAME TO merchant_access;

ALTER TABLE atlas_bpp_dashboard.merchant_access
    ADD COLUMN merchant_id character(36) REFERENCES atlas_bpp_dashboard.merchant (id);

ALTER TABLE atlas_bpp_dashboard.merchant_access DROP COLUMN server_name;

ALTER TABLE atlas_bpp_dashboard.registration_token
    ADD COLUMN merchant_id character(36) REFERENCES atlas_bpp_dashboard.merchant (id);

ALTER TABLE atlas_bpp_dashboard.registration_token DROP COLUMN server_name;
