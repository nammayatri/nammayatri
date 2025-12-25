CREATE TABLE atlas_app.frfs_ticket_booking_breakup ();

ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN quote_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN tag text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN value text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_booking_breakup ADD PRIMARY KEY ( id);
