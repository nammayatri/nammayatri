
------ these migration are according to prod--------------------
insert into atlas_app.client (id, short_id, merchant_id, created_at, updated_at) values ('995ff758-4efa-4d18-8f8f-f779521eb743', 'NAMMA_YATRI','4b17bd06-ae7e-48e9-85bf-282fb310209c',now(),now());
insert into atlas_app.client (id, short_id, merchant_id, created_at, updated_at) values ('cab98477-f759-4467-b936-f6759453bc0b', 'JATRI_SAATHI','bd064716-ae7e-48e9-85bf-282fb310209c',now(),now());
insert into atlas_app.client (id, short_id, merchant_id, created_at, updated_at) values ('c3784e1b-c092-4e97-8175-0e5fffaefc44', 'YATRI','c9811842-d572-11ed-afa1-0242ac120002',now(),now());
insert into atlas_app.client (id, short_id, merchant_id, created_at, updated_at) values ('a791920b-8271-4536-bd6c-14bd4e329812', 'MANA_YATRI','4b17bd06-ae7e-48e9-85bf-282fb310209c',now(),now());
CREATE INDEX idx_client_person_info_person_id ON atlas_app.client_person_info USING btree (person_id);
ALTER TABLE atlas_app.client_person_info
  ADD CONSTRAINT unique_client_person_info_person_id_vehicle_category UNIQUE (person_id, vehicle_category);
