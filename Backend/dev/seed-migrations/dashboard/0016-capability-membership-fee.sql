-- Dedicated capability for creating a driver membership fee order on the driver's
-- behalf with an admin-set amount (PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_MEMBERSHIP_FEE_CREATE).


INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('finance.membership_fee.write', 'finance', 'Create driver membership fee orders with an admin-set amount', false)
ON CONFLICT (id) DO NOTHING;

-- Granted to JUSPAY_ADMIN by role name, so it applies in every environment.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, 'finance.membership_fee.write' FROM atlas_dashboard.role r
WHERE r.name = 'JUSPAY_ADMIN'
ON CONFLICT DO NOTHING;
