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
    ADD COLUMN server_name character varying(255) DEFAULT 'BECKN_TRANSPORT' NOT NULL;

-- WARNING: after this migration all users will have access to BECKN_TRANSPORT
INSERT INTO atlas_bpp_dashboard.server_access (id, person_id, server_name, created_at)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.id,
        'BECKN_TRANSPORT',
        '2022-09-12 15:15:42.104639+00'
    FROM atlas_bpp_dashboard.person AS T1;
    -- INNER JOIN atlas_bpp_dashboard.role AS T2 ON T2.id = T1.role_id
    --     WHERE T2.dashboard_access_type = 'DASHBOARD_ADMIN';
