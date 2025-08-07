CREATE TABLE atlas_app.ticket_assignment ();

ALTER TABLE atlas_app.ticket_assignment ADD COLUMN assigned_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN assigned_by text ;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN assignment_number text NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN assignment_type text NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN ticket_booking_service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_assignment ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_assignment ADD PRIMARY KEY ( id);



------- SQL updates -------


--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_app.ticket_assignment ALTER COLUMN assigned_at DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

