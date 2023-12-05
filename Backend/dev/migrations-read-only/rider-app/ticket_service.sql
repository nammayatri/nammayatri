CREATE TABLE ticket_service ();

ALTER TABLE ticket_service ADD COLUMN allow_future_booking boolean ;
ALTER TABLE ticket_service ADD COLUMN business_hours character varying(36) NOT NULL;
ALTER TABLE ticket_service ADD COLUMN expiry NO_SQL_TYPE ;
ALTER TABLE ticket_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE ticket_service ADD COLUMN max_verification integer ;
ALTER TABLE ticket_service ADD COLUMN operational_days text[] NOT NULL;
ALTER TABLE ticket_service ADD COLUMN place_id text NOT NULL;
ALTER TABLE ticket_service ADD COLUMN service_name text NOT NULL;
ALTER TABLE ticket_service ADD COLUMN short_desc text ;
ALTER TABLE ticket_service ADD PRIMARY KEY ( id);