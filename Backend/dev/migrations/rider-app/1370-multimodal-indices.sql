CREATE INDEX idx_journey_search_request_id ON atlas_app.journey USING btree (search_request_id);

CREATE INDEX idx_journey_rider_id ON atlas_app.journey USING btree (rider_id);

CREATE INDEX idx_journey_booking_journey_id ON atlas_app.journey_booking USING btree (journey_id);

CREATE INDEX idx_journey_leg_journey_id ON atlas_app.journey_leg USING btree (journey_id);

CREATE INDEX idx_journey_feedback_rider_id ON atlas_app.journey_feedback USING btree (rider_id);

CREATE INDEX idx_frfs_ticket_booking_journey_id ON atlas_app.frfs_ticket_booking USING btree (journey_id);
