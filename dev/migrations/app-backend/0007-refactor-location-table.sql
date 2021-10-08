CREATE TABLE atlas_app.location_backup AS
  SELECT * FROM atlas_app.location;

DELETE FROM atlas_app.location WHERE id IN (SELECT location_id FROM atlas_app.organization); 

ALTER TABLE atlas_app.person DROP COLUMN location_id;
ALTER TABLE atlas_app.organization DROP COLUMN location_id;
ALTER TABLE atlas_app.location DROP COLUMN info;
ALTER TABLE atlas_app.location DROP COLUMN ward;
ALTER TABLE atlas_app.location DROP COLUMN point;
ALTER TABLE atlas_app.location DROP COLUMN location_type;
ALTER TABLE atlas_app.location DROP COLUMN bound;

ALTER TABLE atlas_app.location RENAME TO search_request_location;

DELETE FROM atlas_app.search_request_location WHERE lat IS NULL OR long IS NULL;

ALTER TABLE atlas_app.search_request_location ALTER COLUMN lat SET NOT NULL;
ALTER TABLE atlas_app.search_request_location ALTER COLUMN long SET NOT NULL;