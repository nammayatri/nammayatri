CREATE TABLE service_category ();

ALTER TABLE service_category ADD COLUMN allowed_seats integer ;
ALTER TABLE service_category ADD COLUMN available_seats integer ;
ALTER TABLE service_category ADD COLUMN description text NOT NULL;
ALTER TABLE service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE service_category ADD COLUMN name text NOT NULL;
ALTER TABLE service_category ADD COLUMN people_category character varying(36) ;
ALTER TABLE service_category ADD PRIMARY KEY ( id);