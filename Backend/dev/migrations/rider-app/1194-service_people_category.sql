CREATE TABLE atlas_app.service_people_category ();

ALTER TABLE atlas_app.service_people_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN price_per_unit Text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD PRIMARY KEY ( id);
