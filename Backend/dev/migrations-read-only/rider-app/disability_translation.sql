CREATE TABLE atlas_app.disability_translation ();

ALTER TABLE atlas_app.disability_translation ADD COLUMN disability_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.disability_translation ADD COLUMN disability_tag character varying(255) NOT NULL;
ALTER TABLE atlas_app.disability_translation ADD COLUMN language character varying(255) NOT NULL;
ALTER TABLE atlas_app.disability_translation ADD COLUMN translation character varying(255) NOT NULL;
ALTER TABLE atlas_app.disability_translation ADD PRIMARY KEY ( disability_id, language);