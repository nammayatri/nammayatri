-- Run in master and prod .. not for local
ALTER TABLE atlas_app.person_default_emergency_number DROP CONSTRAINT person_default_emergency_number_pkey;
ALTER TABLE atlas_app.person_default_emergency_number ADD PRIMARY KEY ( mobile_number_hash, person_id);