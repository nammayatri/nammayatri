
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT RUN BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
--ALTER TABLE atlas_app.booking DROP COLUMN rental_slab_id;
ALTER TABLE atlas_app.quote DROP COLUMN rental_slab_id;
DROP TABLE IF EXISTS atlas_app.rental_slab;