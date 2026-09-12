

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------

-- {"api":"TransitOperatorQueryVehicle","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.read' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorUpsertVehicles","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.write' ) ON CONFLICT DO NOTHING;

-- {"api":"TransitOperatorDeleteVehicle","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'transit-operations.master.write' ) ON CONFLICT DO NOTHING;
