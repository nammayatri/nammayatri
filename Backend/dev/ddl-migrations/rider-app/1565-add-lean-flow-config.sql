INSERT INTO atlas_app.system_configs (id, config_value)
VALUES ('lean_flow', '{"enabled": false, "featuresExcluded": []}')
ON CONFLICT (id) DO NOTHING;
