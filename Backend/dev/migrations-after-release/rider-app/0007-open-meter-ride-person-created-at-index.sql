-- findAllByPersonIdPaginated filters on person_id and orders by created_at DESC;
-- the generated (created_at, person_id) index cannot seek on person_id.
CREATE INDEX CONCURRENTLY IF NOT EXISTS open_meter_ride_idx_person_id_created_at_desc ON atlas_app.open_meter_ride USING btree (person_id, created_at DESC);
