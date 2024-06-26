CREATE TABLE atlas_app.ambulance_details ();

ALTER TABLE atlas_app.ambulance_details ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_app.ambulance_details ADD COLUMN min_estimated_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.ambulance_details ADD COLUMN max_estimated_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.ambulance_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ambulance_details ADD PRIMARY KEY ( id);
