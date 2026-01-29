CREATE TABLE atlas_app.college ();

ALTER TABLE atlas_app.college ADD COLUMN college_address text ;
ALTER TABLE atlas_app.college ADD COLUMN college_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.college ADD COLUMN contact_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.college ADD COLUMN contact_phone_number_encrypted character varying(255) ;
ALTER TABLE atlas_app.college ADD COLUMN contact_phone_number_hash bytea ;
ALTER TABLE atlas_app.college ADD COLUMN contact_role character varying(255) ;
ALTER TABLE atlas_app.college ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.college ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.college ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.college ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.college ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.college ADD PRIMARY KEY ( id);
