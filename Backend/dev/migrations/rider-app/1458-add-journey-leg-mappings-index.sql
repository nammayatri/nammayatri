CREATE INDEX idx_journey_leg_mapping_journey_id ON atlas_app.journey_leg_mapping USING btree (journey_id);
CREATE INDEX idx_journey_leg_mapping_journey_leg_id ON atlas_app.journey_leg_mapping USING btree (journey_leg_id);
CREATE INDEX idx_journey_leg_group_code ON atlas_app.journey_leg USING btree (group_code);