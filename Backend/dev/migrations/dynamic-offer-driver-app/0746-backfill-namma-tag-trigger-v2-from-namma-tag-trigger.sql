-- Backfill namma_tag_trigger_v2 from namma_tag_trigger: one row per merchant_operating_city
INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (
  merchant_operating_city_id,
  event,
  tag_name,
  created_at,
  updated_at
)
SELECT
  moc.id,
  ntt.event,
  ntt.tag_name,
  ntt.created_at,
  ntt.updated_at
FROM atlas_driver_offer_bpp.namma_tag_trigger ntt
CROSS JOIN atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT (merchant_operating_city_id, event, tag_name) DO NOTHING;
