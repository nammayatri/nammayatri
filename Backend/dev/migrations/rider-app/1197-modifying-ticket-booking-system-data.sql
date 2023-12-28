insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b9000', 'all', '{ebfef970-5e49-0e40-9529-222354e1783a}');
update atlas_app.business_hour set category_id = '{7b03af02-bf06-2c52-3aa6-5042f50b9000}' where id = '885be4e6-dd20-f0d4-0f34-63b655d84327';

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b9001', 'all', '{ebfef970-5e49-0e40-9529-222354e1783a}');
update atlas_app.business_hour set category_id = '{7b03af02-bf06-2c52-3aa6-5042f50b9001}' where id = '485be4e6-dd20-f0d4-0f34-63b655d84327';

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (null, null, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b9003', 'all', '{ebfef970-5e49-0e40-9529-222354e1783b}');
update atlas_app.business_hour set category_id = '{7b03af02-bf06-2c52-3aa6-5042f50b9003}' where id = '685be4e6-dd20-f0d4-0f34-63b655d84327';

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (150, 150, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b9004', 'all', '{cbfef970-5e49-0e40-9529-222354e1783a}');
update atlas_app.business_hour set category_id = '{7b03af02-bf06-2c52-3aa6-5042f50b9004}' where id = '285be4e6-dd20-f0d4-0f34-63b655d84327';

insert into atlas_app.service_category (allowed_seats, available_seats, description, id, name, people_category) values (150, 150, 'all', '7b03af02-bf06-2c52-3aa6-5042f50b9005', 'all', '{cbfef970-5e49-0e40-9529-222354e1783a}');
update atlas_app.business_hour set category_id = '{7b03af02-bf06-2c52-3aa6-5042f50b9005}' where id = '385be4e6-dd20-f0d4-0f34-63b655d84327';

ALTER TABLE atlas_app.ticket_service ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_service ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.ticket_place ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_place ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.ticket_booking ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service ALTER COLUMN merchant_operating_city_id DROP NOT NULL;

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.special_occasion ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.special_occasion ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.special_occasion ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.service_people_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.service_people_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.service_people_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.service_people_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.service_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.service_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.service_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.service_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.seat_management ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.seat_management ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.seat_management ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat_management ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;

ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.business_hour ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
