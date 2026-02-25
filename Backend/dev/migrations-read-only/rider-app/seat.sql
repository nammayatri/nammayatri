CREATE TABLE atlas_app.seat ();

ALTER TABLE atlas_app.seat ADD COLUMN col_no integer NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN is_bookable boolean NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN is_ladies_only boolean ;
ALTER TABLE atlas_app.seat ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN row_no integer NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN seat_label text NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN seat_layout_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.seat ADD COLUMN seat_type text ;
ALTER TABLE atlas_app.seat ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.seat ADD PRIMARY KEY ( id);
