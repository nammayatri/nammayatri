

CREATE INDEX idx_ticket_booking_short_id ON atlas_app.ticket_booking USING btree (short_id);
CREATE INDEX idx_ticket_booking_person_id ON atlas_app.ticket_booking USING btree (person_id);

CREATE INDEX idx_ticket_booking_service_ticket_booking_id ON atlas_app.ticket_booking_service USING btree (ticket_booking_id);
CREATE INDEX idx_ticket_booking_service_service_id ON atlas_app.ticket_booking_service USING btree (ticket_service_id);

CREATE INDEX idx_ticket_booking_service_category_ticket_booking_service_id ON atlas_app.ticket_booking_service_category USING btree (ticket_booking_service_id);
CREATE INDEX idx_ticket_booking_service_category_service_category_id ON atlas_app.ticket_booking_service_category USING btree (service_category_id);

CREATE INDEX idx_ticket_booking_people_category_serviceCategoryId ON atlas_app.ticket_booking_people_category USING btree (ticket_booking_service_category_id);
CREATE INDEX idx_ticket_booking_people_category_people_category_id ON atlas_app.ticket_booking_people_category USING btree (people_category_id);

CREATE INDEX idx_ticket_service_placesId ON atlas_app.ticket_service USING btree (places_id);

CREATE INDEX idx_special_occasion_entityId_date ON atlas_app.special_occasion USING btree (entity_id, date);

CREATE INDEX idx_business_hour_categoryId ON atlas_app.business_hour USING btree (category_id);

CREATE INDEX idx_seat_management_date_categoryId ON atlas_app.seat_management USING btree (date, ticket_service_category_id);

CREATE INDEX idx_ticket_place_merchant_operating_city_id ON atlas_app.ticket_place USING btree (merchant_operating_city_id);