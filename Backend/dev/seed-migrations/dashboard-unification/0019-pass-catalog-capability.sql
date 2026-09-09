-- Pass catalog authoring capability.
--
-- The pass catalog (pass_category -> pass_type -> pass) is city-scoped config:
-- which passes exist, what they cost, what benefits they carry. That is a
-- different grant from `city-operations.pass.*`, which is operating a
-- customer's pass -- STUDENT_PASS_DEPOT holds `city-operations.pass.execute`
-- to verify and activate passes and must not thereby be able to reprice them.
--
-- capability_endpoint rows are emitted by NammaDSL into
-- migrations-read-only/rider-dashboard/API_AppManagement_Pass.sql; they carry a
-- FK to capability(id), so these two rows must exist first.
--
-- No role_capability grants here on purpose: a new capability starts granted to
-- nobody (SUPER_ADMIN break-glass aside). Grant it deliberately to the roles
-- that curate the pass catalog.
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-config.pass_catalog.read', 'city-config', 'Read the pass catalog (categories, types, passes)', false),
    ('city-config.pass_catalog.write', 'city-config', 'Author the pass catalog (categories, types, passes)', false)
ON CONFLICT (id) DO NOTHING;
