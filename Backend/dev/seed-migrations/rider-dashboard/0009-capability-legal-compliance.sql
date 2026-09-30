-- Capability rows for the legal & compliance policy management endpoints
-- (RIDER_MANAGEMENT/POLICY_DOCUMENT/{POST_POLICY_DOCUMENT_CREATE,
--  POST_POLICY_DOCUMENT_UPDATE, GET_POLICY_DOCUMENT_LIST}).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` line in the API spec, but not the capability row itself --
-- that is seeded here, as in 0004.
--
-- Without these rows, API_Management_PolicyDocument.sql fails with
--   capability_endpoint_capability_id_fkey: Key (capability_id)=
--   (system-config.legal_compliance.read) is not present
-- which aborts the whole rider-dashboard migration transaction.

INSERT INTO atlas_bap_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.legal_compliance.read',  'system-config', '', false),
    ('system-config.legal_compliance.write', 'system-config', '', false)
ON CONFLICT (id) DO NOTHING;
