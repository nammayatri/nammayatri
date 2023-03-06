ALTER TABLE atlas_bap_dashboard.transaction ALTER COLUMN requestor_id DROP NOT NULL;

ALTER TABLE atlas_bap_dashboard.transaction ADD COLUMN server_name character varying(255);

UPDATE atlas_bap_dashboard.transaction AS T1 SET server_name = T2.server_name
    FROM atlas_bap_dashboard.merchant as T2 WHERE
    T1.merchant_id = T2.id
