-- {"api":"GetPermitApplicationList","migration":"capability","param":"permit.application.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.application.read', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPermitApplication","migration":"capability","param":"permit.application.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.application.read', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION' ) ON CONFLICT DO NOTHING;

-- {"api":"GetPermitApplicationDocument","migration":"capability","param":"permit.application.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.application.read', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/GET_PERMIT_APPLICATION_DOCUMENT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPermitApplicationDecision","migration":"capability","param":"permit.application.decide","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.application.decide', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DECISION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPermitApplicationDcDecision","migration":"capability","param":"permit.application.dc_decide","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.application.dc_decide', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/POST_PERMIT_APPLICATION_DC_DECISION' ) ON CONFLICT DO NOTHING;

-- {"api":"PostPermitPassVerify","migration":"capability","param":"permit.pass.verify","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'permit.pass.verify', 'DASHBOARD', 'PROVIDER_PERMIT/PERMIT/POST_PERMIT_PASS_VERIFY' ) ON CONFLICT DO NOTHING;
