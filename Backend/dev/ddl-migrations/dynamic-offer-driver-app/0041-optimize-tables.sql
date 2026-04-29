CREATE INDEX idx_booking_provider_id ON atlas_driver_offer_bpp.booking USING btree (provider_id);

CREATE INDEX idx_ride_driver_id_and_status ON atlas_driver_offer_bpp.ride USING btree (driver_id, status);
CREATE INDEX idx_ride_booking_id ON atlas_driver_offer_bpp.ride USING btree (booking_id);

CREATE INDEX idx_person_mobile_num ON atlas_driver_offer_bpp.person USING btree (mobile_country_code, mobile_number_hash);

CREATE INDEX idx_driver_quote_driver_id ON atlas_driver_offer_bpp.driver_quote USING btree (driver_id);

CREATE INDEX idx_driver_quote_search_request_id ON atlas_driver_offer_bpp.driver_quote USING btree (request_id);

CREATE INDEX idx_reg_token_token ON atlas_driver_offer_bpp.registration_token USING btree (token);

CREATE INDEX idx_rider_details_mobile_number ON atlas_driver_offer_bpp.rider_details USING btree (mobile_number_hash);

CREATE INDEX idx_search_request_for_driver_driver_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (driver_id);

CREATE INDEX idx_vehicle_req_num ON atlas_driver_offer_bpp.vehicle USING btree (registration_no);