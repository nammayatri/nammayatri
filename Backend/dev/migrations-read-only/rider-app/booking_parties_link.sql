CREATE TABLE atlas_app.booking_parties_link ();

ALTER TABLE atlas_app.booking_parties_link ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN party_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN party_name text NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN party_type text NOT NULL;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_parties_link ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_parties_link ADD PRIMARY KEY ( id);