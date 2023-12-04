CREATE TABLE atlas_app.special_occasion ();

ALTER TABLE atlas_app.special_occasion ADD COLUMN business_hours text[] NOT NULL; --changed
ALTER TABLE atlas_app.special_occasion ADD COLUMN date date ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN day_of_week text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN description text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN special_day_type date NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD PRIMARY KEY ( id);