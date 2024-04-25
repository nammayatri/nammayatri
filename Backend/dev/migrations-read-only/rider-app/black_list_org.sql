CREATE TABLE atlas_app.black_list_org ();

ALTER TABLE atlas_app.black_list_org ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.black_list_org ADD COLUMN domain character varying(255) NOT NULL;
ALTER TABLE atlas_app.black_list_org ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.black_list_org ADD COLUMN subscriber_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.black_list_org ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.black_list_org ADD PRIMARY KEY ( id);