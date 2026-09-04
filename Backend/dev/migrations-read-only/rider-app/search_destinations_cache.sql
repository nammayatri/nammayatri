CREATE TABLE atlas_app.search_destinations_cache ();

ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN geo_hash text NOT NULL;
ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN response text NOT NULL;
ALTER TABLE atlas_app.search_destinations_cache ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_destinations_cache ADD PRIMARY KEY ( id);
