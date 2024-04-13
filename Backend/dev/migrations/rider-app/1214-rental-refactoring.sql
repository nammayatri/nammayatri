ALTER TABLE atlas_app.booking ADD COLUMN stop_location_id character varying(36);
ALTER TABLE atlas_app.quote ADD COLUMN rental_details_id character varying(36);


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT RUN BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
ALTER TABLE atlas_app.booking DROP COLUMN rental_slab_id;
ALTER TABLE atlas_app.quote DROP COLUMN rental_slab_id;
DROP TABLE IF EXISTS atlas_app.rental_slab;