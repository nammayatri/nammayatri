-- Capability rows for the TDS certificate disbursement endpoints
-- (DRIVER_OFFER_BPP_MANAGEMENT/TDS_DISTRIBUTION/*).
--
-- The generator emits the capability_endpoint and role_capability links from the
-- `migrate: capability:` lines in TdsDistribution.yaml, but not the capability rows
-- themselves -- they are seeded here, as in 0006, 0007 and 0009.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('finance.tds_distribution.read', 'finance', '', false),
    ('finance.tds_distribution.write', 'finance', '', false)
ON CONFLICT (id) DO NOTHING;
