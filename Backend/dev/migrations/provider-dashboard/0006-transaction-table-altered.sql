ALTER TABLE atlas_bpp_dashboard.transaction ALTER COLUMN requestor_id DROP NOT NULL;

ALTER TABLE atlas_bpp_dashboard.transaction ADD COLUMN server_name character varying(255);

UPDATE atlas_bpp_dashboard.transaction AS T1 SET server_name = T2.server_name
    FROM atlas_bpp_dashboard.merchant as T2 WHERE
    T1.merchant_id = T2.id
