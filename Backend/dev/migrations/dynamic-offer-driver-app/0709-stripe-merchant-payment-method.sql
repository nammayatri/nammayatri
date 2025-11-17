-- Run for International track in MASTER and PROD
INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
SELECT
  atlas_driver_offer_bpp.uuid_generate_v4(),
  T1.merchant_id,
  T1.id,
  'ON_FULFILLMENT',
  'Card_DefaultCardType',
  'BAP',
  9
FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
WHERE T1.city IN ('Amsterdam', 'Helsinki')
ON CONFLICT DO NOTHING;

-- ONLY FOR LOCAL --
INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority)
SELECT
  atlas_driver_offer_bpp.uuid_generate_v4(),
  T1.merchant_id,
  T1.id,
  'ON_FULFILLMENT',
  'Card_DefaultCardType',
  'BAP',
  9
FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
WHERE T1.id IN ('favorit0-0000-0000-0000-00000000city', 'nearest-drivers-testing-org00000city')
ON CONFLICT DO NOTHING;
