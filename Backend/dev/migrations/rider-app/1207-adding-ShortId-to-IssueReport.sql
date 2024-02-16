ALTER TABLE atlas_app.issue_report ADD short_id character varying(36);

UPDATE atlas_app.issue_report SET short_id = SUBSTRING(md5(random()::text)::text, 1, 9) || floor(random() * 10)::int WHERE id IS NULL;

ALTER TABLE atlas_app.merchant ADD COLUMN kapture_disposition text;

UPDATE atlas_app.merchant
SET kapture_disposition =
    CASE
        WHEN short_id = 'JATRI_SAATHI' THEN 'YatriSathi_Customer'
        ELSE 'NammaYatri Customer'
    END;

CREATE INDEX idx_issue_report_short_id ON atlas_app.issue_report USING btree (short_id);