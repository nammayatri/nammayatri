UPDATE atlas_app.merchant SET subscriber_id = short_id;

ALTER TABLE atlas_app.merchant ALTER COLUMN subscriber_id SET NOT NULL;