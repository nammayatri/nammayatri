CREATE TABLE atlas_app.special_occasion ();

ALTER TABLE atlas_app.special_occasion ADD COLUMN business_hours text[] NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN date date ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN day_of_week text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN description text ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN special_day_type text NOT NULL;
ALTER TABLE atlas_app.special_occasion ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.special_occasion ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.special_occasion ADD PRIMARY KEY ( id);