

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"PostNammaTagAppDynamicLogicUpdateExperimentGroup","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.dynamic_logic.write' ) ON CONFLICT DO NOTHING;

-- {"api":"PostNammaTagUpdateCustomerTag","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

------- SQL updates -------

-- {"api":"GetNammaTagAppDynamicLogicExperimentGroups","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.dynamic_logic.read' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostNammaTagBulkUpdateCustomerTag","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'system-config.namma_tag.write' ) ON CONFLICT DO NOTHING;
