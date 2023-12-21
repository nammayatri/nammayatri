CREATE TABLE atlas_app.ticket_service ();

ALTER TABLE atlas_app.ticket_service ADD COLUMN allow_future_booking boolean NOT NULL default true;
ALTER TABLE atlas_app.ticket_service ADD COLUMN business_hours text[] NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN enabled boolean ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN expiry text NOT NULL default 'VisitDate 12:00:00';
ALTER TABLE atlas_app.ticket_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN max_verification integer NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN operational_days text[] NOT NULL default '{Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday}';
ALTER TABLE atlas_app.ticket_service ADD COLUMN places_id text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN service text NOT NULL;
ALTER TABLE atlas_app.ticket_service ADD COLUMN short_desc text ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_service ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_service ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_service ADD PRIMARY KEY ( id);