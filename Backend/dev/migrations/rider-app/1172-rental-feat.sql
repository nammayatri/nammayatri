alter table atlas_app.rental_slab add column base_fare int NOT NULL DEFAULT 50; -- for local testing
alter table atlas_app.rental_slab add column per_hour_charge int NOT NULL DEFAULT 1;
alter table atlas_app.rental_slab add column per_hour_free_kms int NOT NULL DEFAULT 2;
alter table atlas_app.rental_slab add column per_extra_km_rate int NOT NULL DEFAULT 3;
alter table atlas_app.rental_slab add column night_shift_charge int NOT NULL DEFAULT 5;
alter table atlas_app.rental_slab drop column base_distance;
alter table atlas_app.rental_slab drop column base_duration;
