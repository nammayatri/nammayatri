CREATE TABLE atlas_driver_offer_bpp.location (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   lat DOUBLE PRECISION NOT NULL,
   lon DOUBLE PRECISION NOT NULL,
   street TEXT,
   door TEXT,
   city TEXT,
   state TEXT,
   country TEXT,
   building TEXT,
   full_address text,
   area_code TEXT,
   area TEXT,
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.location_mapping (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   location_id CHARACTER VARYING(255) NOT NULL,
   tag CHARACTER VARYING(255) NOT NULL,
   entity_id CHARACTER VARYING(255) NOT NULL,
   "order" INTEGER NOT NULL,
   version CHARACTER VARYING(255) NOT NULL
);

CREATE INDEX ON atlas_driver_offer_bpp.location_mapping USING btree (entity_id);


-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO :

-- ALTER TABLE
--    atlas_driver_offer_bpp.search_request DROP COLUMN from_location_id;

-- ALTER TABLE
--    atlas_driver_offer_bpp.search_request DROP COLUMN to_location_id;

-- ALTER TABLE
--    atlas_driver_offer_bpp.booking DROP COLUMN from_location_id;

-- ALTER TABLE
--    atlas_driver_offer_bpp.booking DROP COLUMN to_location_id;


-- ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN from_location_id DROP IF EXISTS NOT NULL;
-- ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN to_location_id DROP IF EXISTS NOT NULL;
-- ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN from_location_id DROP IF EXISTS NOT NULL;
-- ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN to_location_id DROP IF EXISTS NOT NULL;
