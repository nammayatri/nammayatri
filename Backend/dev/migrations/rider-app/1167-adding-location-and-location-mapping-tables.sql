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

-- ALTER TABLE atlas_app.booking ALTER COLUMN from_location_id DROP NOT NULL;
-- ALTER TABLE atlas_app.booking ALTER COLUMN to_location_id DROP NOT NULL;
