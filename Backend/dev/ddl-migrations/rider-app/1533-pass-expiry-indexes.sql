CREATE INDEX CONCURRENTLY IF NOT EXISTS purchased_pass_brin_idx_end_date ON atlas_app.purchased_pass USING brin (end_date);

CREATE INDEX CONCURRENTLY IF NOT EXISTS purchased_pass_partial_idx_person_id_active_or_prebooked ON atlas_app.purchased_pass USING btree (person_id, status) WHERE status IN ('Active', 'PreBooked');
