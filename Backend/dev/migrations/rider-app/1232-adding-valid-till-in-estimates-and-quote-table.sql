ALTER TABLE atlas_app.estimate
ADD COLUMN valid_till timestamp with time zone NOT NULL;

ALTER TABLE atlas_app.quote
ADD COLUMN valid_till timestamp with time zone NOT NULL;

