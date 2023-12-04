CREATE TABLE service_people_category ();

ALTER TABLE service_people_category ADD COLUMN description text NOT NULL;
ALTER TABLE service_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE service_people_category ADD COLUMN name text NOT NULL;
ALTER TABLE service_people_category ADD COLUMN price_per_unit NO_SQL_TYPE NOT NULL;
ALTER TABLE service_people_category ADD PRIMARY KEY ( id);