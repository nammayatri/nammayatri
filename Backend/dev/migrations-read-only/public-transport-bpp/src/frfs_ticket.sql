CREATE TABLE atlas_public_transport_bpp.frfs_ticket ();

ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN bap_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN ticket_qr text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN ticket_status text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_ticket ADD PRIMARY KEY ( id);
