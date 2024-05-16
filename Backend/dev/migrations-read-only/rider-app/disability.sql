CREATE TABLE atlas_app.disability ();

ALTER TABLE atlas_app.disability ADD COLUMN description character varying(255) NOT NULL;
ALTER TABLE atlas_app.disability ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.disability ADD COLUMN tag character varying(255) NOT NULL;
ALTER TABLE atlas_app.disability ADD PRIMARY KEY ( id);