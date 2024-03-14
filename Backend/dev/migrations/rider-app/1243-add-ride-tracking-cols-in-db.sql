ALTER TABLE atlas_app.merchant ADD COLUMN arrived_pickup_threshold double precision default 50;
ALTER TABLE atlas_app.merchant ADD COLUMN driver_on_the_way_notify_expiry integer default 3600;