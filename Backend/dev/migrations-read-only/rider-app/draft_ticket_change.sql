CREATE TABLE atlas_app.draft_ticket_change ();

ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN draft_payload json ;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN is_approval_required boolean NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.draft_ticket_change ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN ticket_merchant_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.draft_ticket_change ADD COLUMN message text ;