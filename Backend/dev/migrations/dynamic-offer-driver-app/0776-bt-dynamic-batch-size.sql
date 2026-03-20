-- Update dynamic_batch_size defaults for Bharat Taxi operating cities
-- Larger batch sizes ensure more drivers receive offers per search round,
-- reducing "no drivers available" outcomes.

-- Delhi CBD: dense area, progressive batch sizes
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET dynamic_batch_size = '{3,5,7,10}'
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'BHARAT_TAXI'
    AND city = 'Delhi'
);

-- NCR suburbs (Noida, Gurugram): wider radius, more batches
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET dynamic_batch_size = '{3,5,7,10}',
    max_radius_of_search = 10000,
    max_number_of_batches = 6
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'BHARAT_TAXI'
    AND city IN ('Noida', 'Gurugram')
);

-- Small cities (Somnath, Rajkot, Dwarka): maximum reach and patience
UPDATE atlas_driver_offer_bpp.driver_pool_config
SET dynamic_batch_size = '{5,7,10,15}',
    max_radius_of_search = 15000,
    max_number_of_batches = 8,
    single_batch_process_time = 180
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'BHARAT_TAXI'
    AND city IN ('Somnath', 'Rajkot', 'Dwarka')
);
