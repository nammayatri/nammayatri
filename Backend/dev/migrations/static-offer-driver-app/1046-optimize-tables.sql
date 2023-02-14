CREATE INDEX idx_booking_provider_id ON atlas_transporter.booking USING btree (provider_id);

CREATE INDEX idx_person_mobile_num ON atlas_transporter.person USING btree (mobile_country_code, mobile_number_hash);

CREATE INDEX idx_quote_search_req_id ON atlas_transporter.quote USING btree (request_id);

CREATE INDEX idx_ride_driver_id_and_status ON atlas_transporter.ride USING btree (driver_id, status);
CREATE INDEX idx_ride_booking_id ON atlas_transporter.ride USING btree (booking_id);

CREATE INDEX idx_reg_token_token ON atlas_transporter.registration_token USING btree (token);

CREATE INDEX idx_rider_details_mobile_number ON atlas_transporter.rider_details USING btree (mobile_number_hash);

CREATE INDEX idx_search_req_msgId_and_provider_id_and_bap_id ON atlas_transporter.search_request USING btree (message_id, provider_id, bap_id);

CREATE INDEX idx_vehicle_req_num ON atlas_transporter.vehicle USING btree (registration_no);