--ALTER TABLE atlas_app.booking ADD COLUMN fulfillment_id text;
ALTER TABLE atlas_app.booking ADD COLUMN driver_id text;
--ALTER TABLE atlas_app.booking ADD COLUMN item_id text NOT NULL DEFAULT '';
ALTER TABLE atlas_app.driver_offer ADD COLUMN driver_id text;
