-- Composite index on (rider_id, status) for findByRiderIdAndStatus —
-- the hot query path on every AFC gate scan (entry/exit).

CREATE INDEX IF NOT EXISTS svp_journey_rider_id_status_idx
  ON atlas_app.svp_journey USING btree (rider_id, status);
