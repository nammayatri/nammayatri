-- Backfill namma_tag_v2 from namma_tag: one row per merchant_operating_city
INSERT INTO atlas_app.namma_tag_v2 (
  merchant_operating_city_id,
  name,
  description,
  category,
  chakra,
  tag_type,
  range_end,
  range_start,
  tags,
  llm_context,
  rule_engine,
  validity,
  action_engine,
  created_at,
  updated_at
)
SELECT
  moc.id,
  nt.name,
  nt.description,
  nt.category,
  nt.chakra,
  nt.tag_type,
  nt.range_end,
  nt.range_start,
  nt.tags,
  nt.llm_context,
  nt.rule_engine,
  nt.validity,
  nt.action_engine,
  nt.created_at,
  nt.updated_at
FROM atlas_app.namma_tag nt
CROSS JOIN atlas_app.merchant_operating_city moc
ON CONFLICT (merchant_operating_city_id, name) DO NOTHING;
