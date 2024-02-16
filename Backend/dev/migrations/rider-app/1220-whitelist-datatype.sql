ALTER TABLE atlas_app.white_list_org ALTER COLUMN subscriber_id TYPE character varying(255);
UPDATE atlas_app.white_list_org SET subscriber_id = trim(subscriber_id);