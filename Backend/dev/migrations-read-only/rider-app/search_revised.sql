CREATE TABLE atlas_app.search_revised ();

ALTER TABLE atlas_app.search_revised ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_revised ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_revised ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_revised ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_revised ADD COLUMN parent_search_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.search_revised ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.search_revised ADD PRIMARY KEY ( id);