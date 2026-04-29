--------- fixed the migration for local not required to run in master ---------------
ALTER TABLE atlas_app.station
ALTER COLUMN merchant_operating_city_id SET DEFAULT 'default_city';
