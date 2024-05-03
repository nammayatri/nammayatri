CREATE TABLE atlas_app.fare_breakup ();

ALTER TABLE atlas_app.fare_breakup ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.fare_breakup ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_app.fare_breakup ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.fare_breakup ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.fare_breakup ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.fare_breakup ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.fare_breakup ALTER COLUMN booking_id TYPE text;
ALTER TABLE atlas_app.fare_breakup ADD COLUMN entity_type text NOT NULL default 'BOOKING';