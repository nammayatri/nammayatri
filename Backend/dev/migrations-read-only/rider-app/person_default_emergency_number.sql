CREATE TABLE atlas_app.person_default_emergency_number ();

ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN contact_person_id text ;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN enable_for_following boolean NOT NULL default false;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN enable_for_share_ride boolean NOT NULL default false;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN merchant_id text ;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN mobile_country_code character varying(255) NOT NULL;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN name character varying(255) NOT NULL;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN priority int NOT NULL default 0;
ALTER TABLE atlas_app.person_default_emergency_number ADD PRIMARY KEY ( mobile_number_hash, person_id);


------- SQL updates -------

ALTER TABLE atlas_app.person_default_emergency_number ADD COLUMN share_trip_with_emergency_contact_option text ;