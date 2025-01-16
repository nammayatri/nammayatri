CREATE TABLE atlas_app.frfs_ticket_discount ();

ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN tnc text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN value text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_discount ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket_discount ADD PRIMARY KEY ( id);