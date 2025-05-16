CREATE TABLE atlas_public_transport_bpp.frfs_payment ();

ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN frfs_ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN payment_reference_number text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_payment ADD PRIMARY KEY ( id);
