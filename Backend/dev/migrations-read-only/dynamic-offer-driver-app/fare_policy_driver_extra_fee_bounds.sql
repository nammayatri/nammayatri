-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN default_step_fee integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN default_step_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN id integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN max_fee integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN max_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN min_fee integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN min_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN start_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN step_fee integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN step_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD PRIMARY KEY ( id);

-- No need to run migrations till here, these are already there !! --