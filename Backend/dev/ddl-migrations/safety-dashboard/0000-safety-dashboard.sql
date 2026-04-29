CREATE TABLE atlas_safety_dashboard.role (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
dashboard_access_type character varying(255) NOT NULL,
description character varying(1024) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16419_primary PRIMARY KEY (id)
,CONSTRAINT  unique_name UNIQUE (name)
);

ALTER TABLE atlas_safety_dashboard.role OWNER TO atlas_safety_dashboard_user;

CREATE TABLE atlas_safety_dashboard.access_matrix (
id character(36) NOT NULL,
role_id character(36) REFERENCES atlas_safety_dashboard.role (id) NOT NULL,
api_entity character varying(255) NOT NULL,
user_access_type character varying(255) NOT NULL,
user_action_type character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
,CONSTRAINT  unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type)
);

ALTER TABLE atlas_safety_dashboard.access_matrix OWNER TO atlas_safety_dashboard_user;

ALTER TABLE atlas_safety_dashboard.person
    ADD COLUMN role_id character(36) DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6' NOT NULL;

ALTER TABLE
   atlas_safety_dashboard.person
ADD
   CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_safety_dashboard.role(id);

CREATE TABLE atlas_safety_dashboard.merchant (
id character(36) NOT NULL,
short_id character varying(255) NOT NULL,
default_operating_city Text  NOT NULL,
is2fa_mandatory Boolean NOT NULL DEFAULT false,
supported_operating_cities Text[] NOT NULL,
server_name character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  merchant_pkey PRIMARY KEY (id),
CONSTRAINT  unique_short_id UNIQUE (short_id)
);

ALTER TABLE atlas_safety_dashboard.merchant OWNER TO atlas_safety_dashboard_user;

CREATE TABLE atlas_safety_dashboard.merchant_access (
id character(36) NOT NULL,
person_id character(36) REFERENCES atlas_safety_dashboard.person (id) NOT NULL,
secret_key character varying(255) ,
is2fa_enabled Boolean NOT NULL DEFAULT false,
is2fa_mandatory Boolean NOT NULL DEFAULT false,
merchant_short_id Text NOT NULL,
merchant_id character(36) NOT NULL REFERENCES atlas_safety_dashboard.merchant (id) NOT NULL,
operating_city Text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16475_primary PRIMARY KEY (id)
,CONSTRAINT  unique_person_id_merchant_id UNIQUE (person_id, merchant_id)
);
ALTER TABLE atlas_safety_dashboard.merchant_access OWNER TO atlas_safety_dashboard_user;

ALTER TABLE atlas_safety_dashboard.registration_token
    ADD COLUMN server_name character varying(255) DEFAULT 'DRIVER_OFFER_BPP' NOT NULL;

ALTER TABLE atlas_safety_dashboard.registration_token
    ADD COLUMN merchant_id character (36) NOT NULL REFERENCES atlas_safety_dashboard.merchant (id) DEFAULT '94bbea0d-3c52-479b-81f5-eca4969ae797';

CREATE TABLE atlas_safety_dashboard.transaction (
id character(36) NOT NULL,
requestor_id character(36) REFERENCES atlas_safety_dashboard.person (id),
merchant_id character(36) REFERENCES atlas_safety_dashboard.merchant (id),
common_driver_id character(36),
common_ride_id character(36),
endpoint character varying(255) NOT NULL,
server_name character varying(255) NOT NULL,
request text,
response text,
response_error text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  idx_transaction_primary PRIMARY KEY (id)
);

ALTER TABLE atlas_safety_dashboard.transaction OWNER TO atlas_safety_dashboard_user;

ALTER TABLE atlas_safety_dashboard.registration_token
ADD COLUMN operating_city Text DEFAULT 'Bangalore' NOT NULL;

ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN server_names Text[];

ALTER TABLE atlas_safety_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;

ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN company_name Text;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN domain Text;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN website Text;
ALTER TABLE atlas_safety_dashboard.merchant ALTER COLUMN server_name DROP NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN auth_token Text;
ALTER TABLE atlas_safety_dashboard.person ADD COLUMN dashboard_access_type Text;
ALTER TABLE atlas_safety_dashboard.registration_token ADD COLUMN enabled BOOLEAN DEFAULT TRUE NOT NULL;

ALTER TABLE atlas_safety_dashboard.merchant DROP COLUMN company_name;

CREATE TABLE atlas_safety_dashboard.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);

CREATE INDEX idx_suspect_dl ON atlas_safety_dashboard.suspect USING btree (dl);
CREATE INDEX idx_suspect_voter_id ON atlas_safety_dashboard.suspect USING btree (voter_id);
CREATE INDEX idx_suspect_flag_request_dl ON atlas_safety_dashboard.suspect_flag_request USING btree (dl);
CREATE INDEX idx_suspect_flag_request_voterId ON atlas_safety_dashboard.suspect_flag_request USING btree (voter_id);
CREATE INDEX idx_suspect_status_history_dl ON atlas_safety_dashboard.suspect_status_history USING btree (dl);
CREATE INDEX idx_suspect_status_history_voterId ON atlas_safety_dashboard.suspect_flag_request USING btree (voter_id);
CREATE INDEX idx_Notification_receiver_id ON atlas_safety_dashboard.notification USING btree (receiver_id);
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN enabled Boolean;