
CREATE INDEX idx_client_person_info_person_id ON atlas_app.client_person_info USING btree (person_id);
ALTER TABLE atlas_app.client_person_info
  ADD CONSTRAINT unique_client_person_info_person_id_vehicle_category UNIQUE (person_id, vehicle_category);
