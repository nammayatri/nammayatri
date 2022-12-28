CREATE TABLE atlas_app.person_flow_status (
    person_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.person (id),
    flow_status JSON,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO atlas_app.person_flow_status (person_id, flow_status)
SELECT T1.id, '{"tag":"IDLE"}'
FROM atlas_app.person as T1;

ALTER TABLE atlas_app.person_flow_status ALTER COLUMN flow_status SET NOT NULL;