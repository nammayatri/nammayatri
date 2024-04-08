CREATE TABLE atlas_app.white_list_org ();

ALTER TABLE atlas_app.white_list_org ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.white_list_org ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN subscriber_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.white_list_org ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.white_list_org ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.white_list_org ALTER COLUMN subscriber_id TYPE character varying(255);
ALTER TABLE atlas_app.white_list_org ALTER COLUMN domain TYPE character varying(255);