CREATE TABLE atlas_app.merchant_operating_city ();

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN merchant_short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_operating_city ADD PRIMARY KEY ( id);