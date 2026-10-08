-- Minimal, additive registration for the RSF Reconciliation dashboard
-- endpoints' capability ids (see Backend/app/dashboard/CommonAPIs/spec/
-- ProviderPlatform/Management/API/RSFReconciliation.yaml). Only the parent
-- `capability` rows are inserted here -- NammaDSL's own generated migration
-- (API_Management_RSFReconciliation.sql) inserts the capability_endpoint
-- rows, which have an FK on capability.id and fail without this. The
-- JUSPAY_ADMIN role is granted all of them so the dashboard can call them.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('financeManagement.rsfOrders.list', 'finance-management', '', false),
    ('financeManagement.rsfOrders.confirm', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.list', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.detail', 'finance-management', '', false),
    ('financeManagement.rsfUtrs.bankVerify', 'finance-management', '', false),
    ('financeManagement.rsfAllocation.auto', 'finance-management', '', false),
    ('financeManagement.rsfSettlements.send', 'finance-management', '', false),
    ('financeManagement.rsfRecon.unmatched', 'finance-management', '', false)
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT '37947162-3b5d-4ed6-bcac-08841be1534d', id
FROM atlas_dashboard.capability
WHERE id IN (
    'financeManagement.rsfOrders.list',
    'financeManagement.rsfOrders.confirm',
    'financeManagement.rsfUtrs.list',
    'financeManagement.rsfUtrs.detail',
    'financeManagement.rsfUtrs.bankVerify',
    'financeManagement.rsfAllocation.auto',
    'financeManagement.rsfSettlements.send',
    'financeManagement.rsfRecon.unmatched'
)
ON CONFLICT DO NOTHING;
