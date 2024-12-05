UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"googleAutocompleteParams"}',
    '["in"]'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_Google'
AND moc.country = 'India';


UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"googleAutocompleteParams"}',
    '["fr", "nl"]'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_Google'
AND moc.country = 'France';


UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"googleAutocompleteParams"}',
    '["us", "pr", "vi", "gu", "mp"]'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_Google'
AND moc.country = 'USA';


UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"mmiAutocompleteParams"}',
    '"ind"'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_MMI'
AND moc.country = 'India';

UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"mmiAutocompleteParams"}',
    '"fr"'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_MMI'
AND moc.country = 'France';

UPDATE atlas_app.merchant_service_config AS msc
SET config_json = jsonb_set(
    config_json::jsonb,
    '{"mmiAutocompleteParams"}',
    '"us"'::jsonb
)::json
FROM atlas_app.merchant_operating_city AS moc
WHERE msc.merchant_operating_city_id = moc.id
AND msc.service_name = 'Maps_MMI'
AND moc.country = 'USA';