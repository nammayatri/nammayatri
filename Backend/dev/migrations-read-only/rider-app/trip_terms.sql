CREATE TABLE atlas_app.trip_terms ();

ALTER TABLE atlas_app.trip_terms ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.trip_terms ADD COLUMN descriptions text NOT NULL;
ALTER TABLE atlas_app.trip_terms ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.trip_terms ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.trip_terms ADD PRIMARY KEY ( id);