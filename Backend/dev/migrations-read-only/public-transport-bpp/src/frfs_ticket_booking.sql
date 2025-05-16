CREATE TABLE atlas_public_transport_bpp.frfs_ticket_booking ();

ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN bap_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN booking_type text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN selected_fare_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket_booking ADD PRIMARY KEY ( id);
