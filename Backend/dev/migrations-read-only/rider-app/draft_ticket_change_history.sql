CREATE TABLE atlas_app.draft_ticket_change_history ();

ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN draft_payload json ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN is_approval_required boolean NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN message text ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN reviewed_by text ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN ticket_merchant_id text ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN ticket_place_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.draft_ticket_change_history ADD PRIMARY KEY ( id);
