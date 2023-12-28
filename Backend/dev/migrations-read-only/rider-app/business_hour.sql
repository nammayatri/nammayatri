CREATE TABLE atlas_app.business_hour ();

ALTER TABLE atlas_app.business_hour ADD COLUMN btype text NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN category_id text[] NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.business_hour ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.business_hour ADD PRIMARY KEY ( id);