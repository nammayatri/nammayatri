ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_lat double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_start_lon double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_lat double precision;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN trip_end_lon double precision;

CREATE TABLE atlas_driver_offer_bpp.transporter_config (
organization_id character(36) NOT NULL,
pickup_loc_threshold bigint,
drop_loc_threshold bigint,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  transporter_config_pkey PRIMARY KEY (organization_id)
,CONSTRAINT  transporter_config_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id)
);
ALTER TABLE atlas_driver_offer_bpp.transporter_config OWNER TO atlas_driver_offer_bpp_user;
