CREATE TABLE atlas_app.email_template ();

ALTER TABLE atlas_app.email_template ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.email_template ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.email_template ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_app.email_template ADD COLUMN merchant_id character(36) NOT NULL;
ALTER TABLE atlas_app.email_template ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_app.email_template ADD COLUMN template_key text NOT NULL;
ALTER TABLE atlas_app.email_template ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.email_template ADD COLUMN value json NOT NULL;
ALTER TABLE atlas_app.email_template ADD PRIMARY KEY ( id);
ALTER TABLE atlas_app.email_template ADD CONSTRAINT email_template_unique_idx_language_merchant_operating_city_id_template_key UNIQUE (language, merchant_operating_city_id, template_key);