-- TEMPORARY LOCAL-ONLY PATCH (2026-08-13): this file was generated as
-- entirely SQL comments ("capability: PUBLIC - nothing to grant locally"
-- for every endpoint) even after RSFReconciliation.yaml's capability values
-- were changed to real dot-notation ids -- the "localAccessForRoleId"
-- migration type did not pick up the change on regeneration (looks like a
-- NammaDSL generator bug specific to this migration type, not something
-- fixable from the YAML). A comment-only migration file crashes the
-- migration runner with "execute: Empty query". Replaced with a real no-op
-- statement to unblock local startup. Will likely be overwritten (back to
-- the buggy comment-only form) on the next `run-generator` -- if so, patch
-- again the same way, or fix the generator itself.
DO $$ BEGIN END $$;


------- SQL updates -------

-- {"api":"GetRSFReconciliationRsfMessages","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSettlements.list' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfMessagesUtrs","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSettlements.utrs' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfMessagesOrders","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSettlements.orders' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfMessagesSend","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfSettlements.send' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtrs","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.list' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfUtr","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.detail' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfUtrBankVerify","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfUtrs.bankVerify' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRSFReconciliationRsfOrdersConfirm","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfOrders.confirm' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconGrid","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfRecon.grid' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRSFReconciliationRsfReconUnmatched","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'financeManagement.rsfRecon.unmatched' ) ON CONFLICT DO NOTHING;
