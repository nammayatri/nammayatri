ALTER TABLE
  atlas_app.ride
ALTER COLUMN
  tracking_url DROP NOT NULL;

UPDATE
    atlas_app.ride
SET
    tracking_url = NULL;
