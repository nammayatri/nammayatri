CREATE TABLE atlas_app.passenger_details ();

ALTER TABLE atlas_app.passenger_details ADD COLUMN age integer ;
ALTER TABLE atlas_app.passenger_details ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_app.passenger_details ADD COLUMN first_name text NOT NULL;
ALTER TABLE atlas_app.passenger_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.passenger_details ADD COLUMN last_name text ;
ALTER TABLE atlas_app.passenger_details ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.passenger_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.passenger_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.passenger_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.passenger_details ADD PRIMARY KEY ( id);
