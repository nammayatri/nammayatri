ALTER TABLE atlas_app.issue_category
ADD COLUMN allowed_ride_statuses text[];

ALTER TABLE atlas_app.issue_option
ADD COLUMN restricted_ride_statuses text[] DEFAULT '{}';