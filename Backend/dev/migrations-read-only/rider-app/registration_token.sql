CREATE TABLE atlas_app.registration_token ();

ALTER TABLE atlas_app.registration_token ADD COLUMN attempts integer NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN auth_expiry integer NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN auth_medium text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN auth_type text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN auth_value_hash text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.registration_token ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN info text ;
ALTER TABLE atlas_app.registration_token ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN token text NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN token_expiry integer NOT NULL;
ALTER TABLE atlas_app.registration_token ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.registration_token ADD COLUMN verified boolean NOT NULL;
ALTER TABLE atlas_app.registration_token ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.registration_token ADD COLUMN created_via_partner_org_id character varying(36) ;