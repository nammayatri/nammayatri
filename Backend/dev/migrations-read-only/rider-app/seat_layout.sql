CREATE TABLE atlas_app.seat_layout ();

ALTER TABLE atlas_app.seat_layout ADD COLUMN columns integer NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN rows integer NOT NULL;
ALTER TABLE atlas_app.seat_layout ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat_layout ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat_layout ADD PRIMARY KEY ( id);
