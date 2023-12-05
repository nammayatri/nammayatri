CREATE TABLE atlas_app.ticket_booking ();

ALTER TABLE atlas_app.ticket_booking ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN created_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN ticket_place_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN updated_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD COLUMN visit_date date NOT NULL;
ALTER TABLE atlas_app.ticket_booking ADD PRIMARY KEY ( id, person_id, short_id);