CREATE TABLE atlas_app.person_disability ();

ALTER TABLE atlas_app.person_disability ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_disability ADD COLUMN description text ;
ALTER TABLE atlas_app.person_disability ADD COLUMN disability_id text NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN tag text NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_disability ADD PRIMARY KEY ( person_id);