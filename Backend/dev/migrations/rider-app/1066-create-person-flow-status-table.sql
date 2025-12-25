INSERT INTO atlas_app.person_flow_status (person_id, flow_status)
SELECT T1.id, '{"tag":"IDLE"}'
FROM atlas_app.person as T1;