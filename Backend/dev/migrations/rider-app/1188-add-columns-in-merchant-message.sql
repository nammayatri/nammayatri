ALTER TABLE atlas_app.merchant_message ADD COLUMN template_id character varying(255);
ALTER TABLE atlas_app.merchant_message ADD COLUMN json_data json;
ALTER TABLE atlas_app.merchant_message ADD COLUMN contains_url_button boolean DEFAULT false;
UPDATE atlas_app.merchant_message SET contains_url_button = false;
ALTER TABLE atlas_app.merchant_message ALTER COLUMN message TYPE text ;