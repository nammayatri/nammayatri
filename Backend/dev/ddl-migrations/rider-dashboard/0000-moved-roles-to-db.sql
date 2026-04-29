CREATE TABLE atlas_bap_dashboard.role (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
dashboard_access_type character varying(255) NOT NULL,
description character varying(1024) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16419_primary PRIMARY KEY (id)
,CONSTRAINT  unique_name UNIQUE (name)
);
ALTER TABLE atlas_bap_dashboard.role OWNER TO atlas_bap_dashboard_user;

CREATE TABLE atlas_bap_dashboard.access_matrix (
id character(36) NOT NULL,
role_id character(36) REFERENCES atlas_bap_dashboard.role (id) NOT NULL,
api_entity character varying(255) NOT NULL,
user_access_type character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
,CONSTRAINT  unique_role_id_api_entity UNIQUE (role_id, api_entity)
);
ALTER TABLE atlas_bap_dashboard.access_matrix OWNER TO atlas_bap_dashboard_user;

ALTER TABLE atlas_bap_dashboard.person
    ADD COLUMN role_id character(36);

ALTER TABLE
   atlas_bap_dashboard.person
ADD
   CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_bap_dashboard.role(id);
ALTER TABLE atlas_bap_dashboard.person
  DROP COLUMN role;
