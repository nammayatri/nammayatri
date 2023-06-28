CREATE TABLE atlas_driver_offer_bpp.location (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   lat DOUBLE PRECISION NOT NULL,
   lon DOUBLE PRECISION NOT NULL,
   street CHARACTER VARYING(255),
   door CHARACTER VARYING(255),
   city CHARACTER VARYING(255),
   state CHARACTER VARYING(255),
   country CHARACTER VARYING(255),
   building CHARACTER VARYING(255),
   full_address CHARACTER VARYING(255),
   area_code CHARACTER VARYING(255),
   area CHARACTER VARYING(255),
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.location_mapping (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   location_id CHARACTER VARYING(255) NOT NULL,
   tag CHARACTER VARYING(255) NOT NULL,
   tag_id CHARACTER VARYING(255) NOT NULL,
   "order" INTEGER NOT NULL,
   version CHARACTER VARYING(255) NOT NULL
);

ALTER TABLE
   atlas_driver_offer_bpp.search_request DROP COLUMN from_location_id;

ALTER TABLE
   atlas_driver_offer_bpp.search_request DROP COLUMN to_location_id;

ALTER TABLE
   atlas_driver_offer_bpp.booking DROP COLUMN from_location_id;

ALTER TABLE
   atlas_driver_offer_bpp.booking DROP COLUMN to_location_id;
