ALTER TABLE atlas_app.booking ADD COLUMN stop_location_id character varying(36);
ALTER TABLE atlas_app.quote ADD COLUMN rental_details_id character varying(36);

CREATE TABLE atlas_app.rental_details ();
ALTER TABLE atlas_app.rental_details ADD COLUMN base_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_hour_charge numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_hour_free_kms integer NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN per_extra_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_charge numeric(30, 2);
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_start time without time zone;
ALTER TABLE atlas_app.rental_details ADD COLUMN night_shift_end time without time zone;

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
-- @ WARNING: DO NOT RUN BEFORE FULL RELEASE - DROP QUERY ZONE @ --
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ --
ALTER TABLE atlas_app.booking DROP COLUMN rental_slab_id;
ALTER TABLE atlas_app.quote DROP COLUMN rental_slab_id;
DROP TABLE IF EXISTS atlas_app.rental_slab;