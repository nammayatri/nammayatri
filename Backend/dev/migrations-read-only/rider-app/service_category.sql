CREATE TABLE atlas_app.service_category ();

ALTER TABLE atlas_app.service_category ADD COLUMN allowed_seats integer ;
ALTER TABLE atlas_app.service_category ADD COLUMN available_seats integer ;
ALTER TABLE atlas_app.service_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.service_category ADD COLUMN people_category text[] ;
ALTER TABLE atlas_app.service_category ADD PRIMARY KEY ( id);