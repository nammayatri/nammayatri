CREATE TABLE atlas_app.pass_category ();

ALTER TABLE atlas_app.pass_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.pass_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_category ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_category ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.pass_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass_category ADD PRIMARY KEY ( id);
