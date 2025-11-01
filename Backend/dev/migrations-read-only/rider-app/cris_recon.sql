CREATE TABLE atlas_app.cris_recon ();

ALTER TABLE atlas_app.cris_recon ADD COLUMN bpp_order_id text NOT NULL;
ALTER TABLE atlas_app.cris_recon ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cris_recon ADD COLUMN date_ist date NOT NULL;
ALTER TABLE atlas_app.cris_recon ADD COLUMN fare_amount double precision NOT NULL;
ALTER TABLE atlas_app.cris_recon ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.cris_recon ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.cris_recon ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.cris_recon ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.cris_recon ADD PRIMARY KEY ( id);
