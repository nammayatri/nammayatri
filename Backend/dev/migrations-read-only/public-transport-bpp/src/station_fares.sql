CREATE TABLE atlas_public_transport_bpp.station_fares ();

ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN fare_amount double precision NOT NULL;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN from_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN to_station_id character varying(36) NOT NULL;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_public_transport_bpp.station_fares ADD PRIMARY KEY ( id);
