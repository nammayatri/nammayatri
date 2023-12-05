DROP TABLE IF EXISTS atlas_app.seat_management;

CREATE TABLE atlas_app.seat_management ();

ALTER TABLE atlas_app.seat_management ADD COLUMN blocked integer NOT NULL default 0;
ALTER TABLE atlas_app.seat_management ADD COLUMN booked integer NOT NULL default 0;
ALTER TABLE atlas_app.seat_management ADD COLUMN date date NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD COLUMN ticket_service_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_management ADD PRIMARY KEY ( id);