-- Grant driver_issue capabilities to MSIL_ADMIN role for IGM dashboard APIs
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.driver_issue.read'), ('city-operations.driver_issue.write')) AS c(cap)
WHERE r.name = 'MSIL_ADMIN' ON CONFLICT DO NOTHING;
