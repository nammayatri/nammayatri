ALTER TABLE atlas_app.merchant ADD COLUMN subscriber_id character(36);

UPDATE atlas_app.merchant SET subscriber_id = short_id;

ALTER TABLE atlas_app.merchant ALTER COLUMN subscriber_id SET NOT NULL;

ALTER TABLE atlas_app.black_list_org RENAME COLUMN short_id TO subscriber_id;