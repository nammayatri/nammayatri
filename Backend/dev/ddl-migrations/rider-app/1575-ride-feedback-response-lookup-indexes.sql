-- During-ride feedback: lookups by ride (questions / submitted answers) and by rider history (cooldown).
CREATE INDEX IF NOT EXISTS idx_ride_feedback_response_ride_id ON atlas_app.ride_feedback_response USING btree (ride_id);
CREATE INDEX IF NOT EXISTS idx_ride_feedback_response_person_id_created_at ON atlas_app.ride_feedback_response USING btree (person_id, created_at);
