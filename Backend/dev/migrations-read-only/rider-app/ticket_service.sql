CREATE TABLE atlas_app.ticket_service ();

ALTER TABLE atlas_app.ticket_service ADD COLUMN allow_future_booking boolean ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN business_hours text[] NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN expiry text ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN max_verification integer ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN operational_days text[] NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN place_id text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN short_desc text ;
ALTER TABLE atlas_app.ticket_service ADD PRIMARY KEY ( id);