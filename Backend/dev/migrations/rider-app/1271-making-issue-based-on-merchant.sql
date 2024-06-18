ALTER TABLE atlas_app.issue_category
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.issue_config
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.issue_message
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.issue_option
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.issue_report
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.issue_translation
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';

ALTER TABLE atlas_app.comment
ADD COLUMN merchant_id text NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52';