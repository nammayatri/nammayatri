-- QUERIES FOR PROD AND LOCAL
ALTER TABLE atlas_app.issue_category
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.issue_config
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.issue_message
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.issue_option
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.issue_report
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.issue_translation
ADD COLUMN merchant_id character varying (36);

ALTER TABLE atlas_app.comment
ADD COLUMN merchant_id character varying (36);

--QUERIES FOR LOCAL AND PRODUCTION
ALTER TABLE atlas_app.issue_category
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_app.issue_config
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_app.issue_message
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_app.issue_option
ALTER COLUMN merchant_id SET NOT NULL;

ALTER TABLE atlas_app.issue_translation
ALTER COLUMN merchant_id SET NOT NULL;
