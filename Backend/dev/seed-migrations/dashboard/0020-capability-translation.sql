-- Capability rows for the translation dashboard APIs
-- ({PROVIDER,RIDER}_MANAGEMENT/TRANSLATION/{GET_TRANSLATION_DOMAINS, GET_TRANSLATION_KEYS}).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` lines in Translation.yaml, but not the capability rows
-- themselves -- those are seeded here, as in 0006, 0007, 0009 and 0019.
--
-- Without these rows, API_Management_Translation.sql fails with
--   capability_endpoint_capability_id_fkey: Key (capability_id)=
--   (system-config.translation.read) is not present
-- which aborts the whole dashboard migration transaction.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.translation.read', 'system-config', '', false)
ON CONFLICT (id) DO NOTHING;

-- The provider-side endpoint rows. The generator keys a row by api name and capability only,
-- and both platforms' Translation.yaml declare the same endpoints, so API_Management_Translation.sql
-- carries the rider rows alone; the provider rows are seeded here, as in 0004. Tools.Auth.Capability
-- fails closed, so without them the driver dashboard's translation pages return 403 for everyone.
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('system-config.translation.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_DOMAINS'),
    ('system-config.translation.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TRANSLATION/GET_TRANSLATION_KEYS')
ON CONFLICT DO NOTHING;

-- Granted to JUSPAY_ADMIN by role name, so it applies in every environment.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('system-config.translation.read')) AS c(cap)
WHERE r.name = 'JUSPAY_ADMIN'
ON CONFLICT DO NOTHING;
