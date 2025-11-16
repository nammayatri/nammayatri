CREATE TABLE atlas_app.pass_type ();

ALTER TABLE atlas_app.pass_type ADD COLUMN catchline text ;
ALTER TABLE atlas_app.pass_type ADD COLUMN description text ;
ALTER TABLE atlas_app.pass_type ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN name text ;
ALTER TABLE atlas_app.pass_type ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN pass_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_app.pass_type ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_type ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_type ADD PRIMARY KEY ( id);
