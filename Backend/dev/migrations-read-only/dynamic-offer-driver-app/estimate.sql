CREATE TABLE atlas_driver_offer_bpp.estimate ();

ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_params_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN fare_policy_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN is_scheduled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN max_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN min_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN trip_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD PRIMARY KEY ( id);