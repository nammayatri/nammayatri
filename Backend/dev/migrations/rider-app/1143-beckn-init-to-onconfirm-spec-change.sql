ALTER TABLE atlas_app.booking ADD COLUMN fulfillment_id text;
ALTER TABLE atlas_app.booking ADD COLUMN driver_id text;
ALTER TABLE atlas_app.booking ADD COLUMN provider_short_id text;
ALTER TABLE atlas_app.driver_offer ADD COLUMN driver_id text;
ALTER TABLE atlas_app.quote ADD COLUMN provider_short_id text;
