CREATE TABLE atlas_app.seat_management ();

ALTER TABLE atlas_app.seat_management ADD COLUMN blocked integer NOT NULL default 0;
ALTER TABLE atlas_app.seat_management ADD COLUMN booked integer NOT NULL default 0;
ALTER TABLE atlas_app.seat_management ADD COLUMN date date NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN ticket_service_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.seat_management ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.seat_management ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat_management ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat_management ADD PRIMARY KEY ( id);