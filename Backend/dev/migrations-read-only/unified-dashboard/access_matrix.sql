CREATE TABLE atlas_dashboard.access_matrix ();

ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN role_id character varying(36) NOT NULL;
ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN server_name text ;
ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_dashboard.access_matrix ADD COLUMN user_action_type text NOT NULL;
ALTER TABLE atlas_dashboard.access_matrix ADD PRIMARY KEY ( id);
