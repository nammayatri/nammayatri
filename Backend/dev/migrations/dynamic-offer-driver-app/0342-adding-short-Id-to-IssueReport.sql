ALTER TABLE atlas_driver_offer_bpp.issue_report ADD short_id character varying(36);

UPDATE atlas_driver_offer_bpp.issue_report SET short_id = SUBSTRING(md5(random()::text)::text, 1, 9) || floor(random() * 10)::int IS NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN kapture_disposition text;

UPDATE atlas_driver_offer_bpp.transporter_config
SET kapture_disposition =
    CASE
        WHEN m.short_id = 'JATRI_SAATHI_PARTNER' THEN 'YatriSathi Driver'
        ELSE 'NammaYatri Driver'
    END
FROM atlas_driver_offer_bpp.merchant m
WHERE atlas_driver_offer_bpp.transporter_config.merchant_id = m.id;

CREATE INDEX idx_issue_report_short_id ON atlas_driver_offer_bpp.issue_report USING btree (short_id);