-- Give the fleet owner dashboard role (FLEET) every finance-related capability, so a fleet owner can
-- use the wallet, payout, subscription, ledger/invoice/report and RSF reconciliation APIs.
-- Non-finance capabilities (system config, onboarding, ride/driver writes) are not granted here.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT 'e5a69a26-d165-455a-a711-33a41e0d4812', c.capability_id
FROM (
  SELECT DISTINCT capability_id FROM atlas_dashboard.capability_endpoint
  WHERE capability_id LIKE 'finance.%'
     OR capability_id LIKE 'financeManagement.%'
     OR capability_id IN (
       'city-operations.wallet.read',
       'city-operations.wallet.write',
       'city-operations.subscription.read',
       'city-operations.subscription.write'
     )
) c
ON CONFLICT DO NOTHING;
