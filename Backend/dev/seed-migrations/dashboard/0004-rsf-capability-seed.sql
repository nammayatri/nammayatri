-- Minimal, additive registration for the RSF Reconciliation dashboard
-- endpoints' capability ids (see Backend/app/dashboard/CommonAPIs/spec/
-- ProviderPlatform/Management/API/RSFReconciliation.yaml). Only the parent
-- `capability` rows are inserted here -- NammaDSL's own generated migration
-- (API_Management_RSFReconciliation.sql) inserts the capability_endpoint
-- rows, which have an FK on capability.id and fail without this. No
-- role_capability grants are seeded -- testing currently relies on the
-- SUPER_ADMIN break-glass path (Tools.Auth.Capability.enforce), which
-- bypasses capability checks entirely, so no role needs to hold these yet.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('financeManagement.rsfSettlements.list', 'finance-management', '', false),
    ('financeManagement.rsfSettlements.utrs', 'finance-management', '', false),
    ('financeManagement.rsfSettlements.orders', 'finance-management', '', false),
    ('financeManagement.rsfSettlements.send', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.list', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.detail', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.bankVerify', 'finance-management', '', false),
    ('financeManagement.rsfOrders.confirm', 'finance-management', '', false),
    ('financeManagement.rsfRecon.grid', 'finance-management', '', false),
    ('financeManagement.rsfRecon.unmatched', 'finance-management', '', false)
ON CONFLICT (id) DO NOTHING;
