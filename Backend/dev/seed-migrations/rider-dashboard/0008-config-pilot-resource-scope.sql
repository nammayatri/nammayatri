
BEGIN;

UPDATE atlas_bap_dashboard.capability
   SET resource_type = 'CONFIG'
 WHERE id IN ('system-config.config_pilot.read', 'system-config.config_pilot.write');

INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_VERIFY'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_UPSERT_LOGIC_ROLLOUT'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_VERIFY'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_UPSERT_LOGIC_ROLLOUT'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ROLLOUT_ACTION'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ROLLOUT_ACTION')
ON CONFLICT DO NOTHING;

-- Where each endpoint carries the config.

UPDATE atlas_bap_dashboard.capability_endpoint ce
   SET resource_id_param = b.binding
  FROM (VALUES
         ('GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS', 'param:configPilot'),
         ('GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA', 'param:configPilot'),
         ('GET_NAMMA_TAG_CONFIG_PILOT_GET_DIMENSION_SCHEMA', 'param:configPilot'),
         ('GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS', '__SKIP__'),
         ('POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG_WITH_DIMENSIONS', 'param:configPilot'),
         ('POST_NAMMA_TAG_CONFIG_PILOT_CREATE_ROW', 'param:configPilot'),
         ('POST_NAMMA_TAG_CONFIG_PILOT_VERIFY', 'param:configPilot'),
         ('POST_NAMMA_TAG_CONFIG_PILOT_UPSERT_LOGIC_ROLLOUT', 'param:configPilot'),
         ('POST_NAMMA_TAG_CONFIG_PILOT_ROLLOUT_ACTION', 'param:configPilot')
       ) AS b(action, binding)
 WHERE ce.capability_id IN ('system-config.config_pilot.read', 'system-config.config_pilot.write')
   AND split_part(ce.endpoint_id, '/', 3) = b.action;

COMMIT;

-- Verification -- expect nine bound rows per platform prefix, and NULL on every
-- other ConfigPilot endpoint:
--
--   SELECT ce.endpoint_id, ce.capability_id, ce.resource_id_param
--     FROM atlas_bap_dashboard.capability_endpoint ce
--    WHERE split_part(ce.endpoint_id, '/', 3) LIKE '%CONFIG\_PILOT\_%'
--    ORDER BY ce.resource_id_param NULLS LAST, ce.endpoint_id;
