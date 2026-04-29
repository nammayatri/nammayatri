

ALTER TABLE atlas_app.estimate
  ALTER COLUMN min_total_fare SET NOT NULL;

ALTER TABLE atlas_app.estimate
  ALTER COLUMN max_total_fare SET NOT NULL;
