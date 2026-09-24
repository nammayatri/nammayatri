-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN base_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN fare_policy_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN id integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN night_shift_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_cgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_sgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN start_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN free_wating_time text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN waiting_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD PRIMARY KEY ( id);

-- No need to run migrations till here, these are already there !! --