CREATE TABLE atlas_public_transport_bpp.frfs_search_request ();

ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN bap_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN bap_uri text ;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN bpp_id text NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN destination_station_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN quantity integer NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN source_station_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.frfs_search_request ADD PRIMARY KEY ( transaction_id);
