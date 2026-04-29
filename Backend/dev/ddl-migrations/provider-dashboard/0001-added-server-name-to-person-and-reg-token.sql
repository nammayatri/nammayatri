CREATE TABLE atlas_bpp_dashboard.server_access (
id character(36) NOT NULL,
person_id character(36) REFERENCES atlas_bpp_dashboard.person (id) NOT NULL,
server_name character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16475_primary PRIMARY KEY (id)
,CONSTRAINT  unique_person_id_server_name UNIQUE (person_id, server_name)
);
ALTER TABLE atlas_bpp_dashboard.server_access OWNER TO atlas_bpp_dashboard_user;

ALTER TABLE atlas_bpp_dashboard.registration_token
    ADD COLUMN server_name character varying(255) DEFAULT 'DRIVER_OFFER_BPP' NOT NULL;

