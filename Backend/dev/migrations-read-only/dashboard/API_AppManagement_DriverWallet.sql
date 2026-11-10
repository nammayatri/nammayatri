-- {"api":"GetDriverWalletWalletLedgerEntries","migration":"capability","param":"city-operations.wallet.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_LEDGER_ENTRIES' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetDriverWalletWalletLedgerHistory","migration":"capability","param":"city-operations.wallet.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_LEDGER_HISTORY' ) ON CONFLICT DO NOTHING;
