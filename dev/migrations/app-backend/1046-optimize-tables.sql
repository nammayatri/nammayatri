CREATE INDEX idx_booking_rider_id ON atlas_app.booking USING btree (rider_id);
CREATE INDEX idx_booking_rider_id_and_status ON atlas_app.booking USING btree (rider_id, status);

CREATE INDEX idx_estimate_request_id ON atlas_app.estimate USING btree (request_id);

CREATE INDEX idx_fare_breakup_booking_id ON atlas_app.fare_breakup USING btree (booking_id);

CREATE INDEX idx_quote_search_req_id ON atlas_app.quote USING btree (request_id);
CREATE INDEX idx_quote_provider_id ON atlas_app.quote USING btree (provider_id);

CREATE INDEX idx_person_mobile_num ON atlas_app.person USING btree (mobile_country_code, mobile_number_hash);

CREATE INDEX idx_ride_bpp_ride_id_and_status ON atlas_app.ride USING btree (bpp_ride_id);
CREATE INDEX idx_ride_booking_id ON atlas_app.ride USING btree (booking_id);

CREATE INDEX idx_reg_token_token ON atlas_app.registration_token USING btree (token);

CREATE INDEX idx_search_req_rider_id ON atlas_app.search_request USING btree (rider_id);