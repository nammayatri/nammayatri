CREATE TABLE atlas_app.multi_modal_calendar ();

ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN end_date date NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN friday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN monday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN saturday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN start_date date NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN sunday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN thursday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN tuesday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN wednesday text NOT NULL;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_calendar ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.multi_modal_calendar ADD PRIMARY KEY ( id);