CREATE TABLE special_occasion ();

ALTER TABLE special_occasion ADD COLUMN business_hours character varying(36) NOT NULL;
ALTER TABLE special_occasion ADD COLUMN date date ;
ALTER TABLE special_occasion ADD COLUMN day_of_week text ;
ALTER TABLE special_occasion ADD COLUMN description text ;
ALTER TABLE special_occasion ADD COLUMN entity_id text NOT NULL;
ALTER TABLE special_occasion ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE special_occasion ADD COLUMN special_day_type date NOT NULL;
ALTER TABLE special_occasion ADD PRIMARY KEY ( id);