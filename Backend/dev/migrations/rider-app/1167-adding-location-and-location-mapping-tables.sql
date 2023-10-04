CREATE TABLE atlas_app.location (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   lat DOUBLE PRECISION NOT NULL,
   lon DOUBLE PRECISION NOT NULL,
   street TEXT,
   door TEXT,
   city TEXT,
   state TEXT,
   country TEXT,
   building TEXT,
   area_code TEXT,
   area TEXT,
   ward TEXT,
   place_id TEXT,
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_app.location_mapping (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   location_id CHARACTER VARYING(255) NOT NULL,
   tag CHARACTER VARYING(255) NOT NULL,
   entity_id CHARACTER VARYING(255) NOT NULL,
   "order" INTEGER NOT NULL,
   version CHARACTER VARYING(255) NOT NULL
);

CREATE INDEX ON atlas_app.location_mapping USING btree (entity_id);

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO :

-- ALTER TABLE
--    atlas_app.search_request DROP COLUMN from_location_id;

-- ALTER TABLE
--    atlas_app.search_request DROP COLUMN to_location_id;

-- ALTER TABLE
--    atlas_app.booking DROP COLUMN from_location_id;

-- ALTER TABLE
--    atlas_app.booking DROP COLUMN to_location_id;

ALTER TABLE atlas_app.search_request ALTER COLUMN from_location_id DROP NOT NULL;
ALTER TABLE atlas_app.search_request ALTER COLUMN to_location_id DROP NOT NULL;
ALTER TABLE atlas_app.booking ALTER COLUMN from_location_id DROP NOT NULL;
ALTER TABLE atlas_app.booking ALTER COLUMN to_location_id DROP NOT NULL;
