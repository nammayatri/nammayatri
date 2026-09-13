CREATE TABLE atlas_app.cohort_details ();

ALTER TABLE atlas_app.cohort_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cohort_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cohort_details ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.cohort_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cohort_details ADD PRIMARY KEY ( id);
