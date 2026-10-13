-- Driver wallet (GST collection via the wallet): config + pending-dues migration endpoints in
-- Management/API/Merchant.yaml. The generator emits the capability_endpoint links from the
-- `migrate: capability:` lines; this seeds the capability rows they point to.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('system-config.driver_wallet.read', 'system-config', 'View driver wallet config and dues migration status', false),
    ('system-config.driver_wallet.write', 'system-config', 'Update driver wallet config and move pending dues into wallets', false)
ON CONFLICT (id) DO NOTHING;
