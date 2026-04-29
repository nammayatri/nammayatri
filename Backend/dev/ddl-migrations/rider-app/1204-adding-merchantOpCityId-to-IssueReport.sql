ALTER TABLE atlas_app.issue_report
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);