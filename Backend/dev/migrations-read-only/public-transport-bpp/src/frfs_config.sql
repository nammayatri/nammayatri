CREATE TABLE atlas_public_transport_bpp.frfs_config ();

ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN config text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_config ADD PRIMARY KEY ( id);
