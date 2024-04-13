CREATE TABLE atlas_app.person_disability ();

ALTER TABLE atlas_app.person_disability ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_disability ADD COLUMN description character varying(255) ;
ALTER TABLE atlas_app.person_disability ADD COLUMN disability_id character(36) NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN tag character varying(255) NOT NULL;
ALTER TABLE atlas_app.person_disability ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_disability ADD PRIMARY KEY ( person_id);