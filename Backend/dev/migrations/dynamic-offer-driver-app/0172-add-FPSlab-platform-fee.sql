ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_charge integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_cgst integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN platform_fee_sgst integer;

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_slab_details (
  fare_parameters_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.fare_parameters(id),
  platform_fee integer
);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details OWNER TO atlas_driver_offer_bpp_user;

WITH SlabFareParameters AS (
  SELECT T1.id, CAST (null AS Integer)
  FROM atlas_driver_offer_bpp.fare_parameters AS T1
  WHERE T1.fare_parameters_type = 'Slab'
)
INSERT INTO atlas_driver_offer_bpp.fare_parameters_slab_details (
  fare_parameters_id,
  platform_fee)
  (SELECT * FROM SlabFareParameters);