CREATE TABLE IF NOT EXISTS  atlas_driver_offer_bpp.place_name_cache(
  id character(36) NOT NULL PRIMARY KEY,
  formatted_address TEXT,
  plus_code TEXT,
  lat DOUBLE PRECISION NOT NULL,
  lon DOUBLE PRECISION NOT NULL,
  place_id varchar (255) NOT NULL,
  address_components Text[] NOT NULL,
  geo_hash TEXT NOT  NULL,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);