
-- update atlas_app.rider_config set make_multi_modal_search = true;

CREATE INDEX idx_journey_leg_journey_id ON atlas_app.journey_leg USING btree (journey_id);
CREATE INDEX idx_journey_search_request_id ON atlas_app.journey USING btree (search_request_id);