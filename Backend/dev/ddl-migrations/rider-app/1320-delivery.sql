-- Creating idx on parties tables
CREATE INDEX idx_search_request_id ON atlas_app.search_request_parties_link USING btree (search_request_id);

CREATE INDEX idx_booking_id ON atlas_app.booking_parties_link USING btree (booking_id);
CREATE INDEX idx_party_id_and_is_active ON atlas_app.booking_parties_link USING btree (party_id, is_active);
