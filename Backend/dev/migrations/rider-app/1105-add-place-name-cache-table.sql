CREATE TABLE IF NOT EXISTS atlas_app.place_name_cache(
  id character(36) NOT NULL PRIMARY KEY,
  formatted_address TEXT,
  plus_code TEXT,
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL,
  place_id varchar (255) NOT NULL,
  address_components Text[] NOT NULL,
  geo_hash TEXT NOT  NULL
);
ALTER TABLE atlas_app.merchant ADD COLUMN geo_hash_precision_value INT NOT NULL DEFAULT 8;