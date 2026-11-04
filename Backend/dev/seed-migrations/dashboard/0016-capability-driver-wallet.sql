-- Driver wallet (GST collection via the wallet): the read-only config view and the pending-dues
-- migration endpoint in Management/API/Merchant.yaml. The generator emits the capability_endpoint
-- links from their `migrate: capability:` lines; this seeds the capability rows they point to. The
-- wallet config itself is edited through ConfigPilot (TransporterConfig), under the ConfigPilot
-- capabilities.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.driver_wallet.read', 'system-config', 'View the driver wallet config', false),
    ('system-config.driver_wallet.write', 'system-config', 'Move a city''s pending subscription dues into the driver wallets', false)
ON CONFLICT (id) DO NOTHING;
