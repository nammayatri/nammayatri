ALTER TABLE atlas_app.estimate ADD COLUMN min_total_fare numeric(30,2);
ALTER TABLE atlas_app.estimate ADD COLUMN max_total_fare numeric(30,2);

UPDATE atlas_app.estimate
    SET min_total_fare = estimated_total_fare,
        max_total_fare = estimated_total_fare;

ALTER TABLE atlas_app.estimate
  ALTER COLUMN min_total_fare SET NOT NULL;

ALTER TABLE atlas_app.estimate
  ALTER COLUMN max_total_fare SET NOT NULL;
