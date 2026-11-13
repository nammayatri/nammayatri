-- Points a fare policy at a set of distance-based platform fee slabs (platform_fee_slab.slab_set_id).
-- NULL keeps the flat fare_policy.platform_fee, so this is a no-op until a policy opts in.
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN platform_fee_slab_set_id character varying(36);

-- Reference only (ids are per environment) -- seed one slab set, then point every Intercity fare
-- policy of the city at it:
--
-- INSERT INTO atlas_driver_offer_bpp.platform_fee_slab
--   (slab_set_id, merchant_operating_city_id, min_distance_meters, max_distance_meters, platform_fee, cgst_percentage, sgst_percentage)
-- VALUES
--   ('<slab_set_id>', '<merchant_operating_city_id>', 0, 50000, 45, 0.09, 0.09),
--   ('<slab_set_id>', '<merchant_operating_city_id>', 50000, 100000, 100, 0.09, 0.09),
--   ('<slab_set_id>', '<merchant_operating_city_id>', 100000, NULL, 200, 0.09, 0.09);
--
-- UPDATE atlas_driver_offer_bpp.fare_policy SET platform_fee_slab_set_id = '<slab_set_id>'
-- WHERE id IN (
--   SELECT fare_policy_id FROM atlas_driver_offer_bpp.fare_product
--   WHERE merchant_operating_city_id = '<merchant_operating_city_id>' AND trip_category LIKE 'InterCity%'
-- );
