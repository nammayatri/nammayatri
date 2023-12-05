CREATE TABLE business_hour ();

ALTER TABLE business_hour ADD COLUMN btype NO_SQL_TYPE NOT NULL;
ALTER TABLE business_hour ADD COLUMN category_id character varying(36) NOT NULL;
ALTER TABLE business_hour ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE business_hour ADD PRIMARY KEY ( id);