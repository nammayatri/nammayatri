-- Please update auth key with prod credentials before deploying
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Ticket_Kapture',
  json_build_object(
    'createAuth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
    'updateAuth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
    'version', 'v.2.0',
    'url', 'nammayatri.kapturecrm.com'
    )
FROM atlas_driver_offer_bpp.merchant m;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN issue_ticket_service character varying(30) DEFAULT 'Kapture' NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN ticket_id character varying(255);
UPDATE atlas_driver_offer_bpp.issue_category SET category = 'fare' WHERE id = '5ca814d9-66e2-4ccc-b236-40b73b705e88';
UPDATE atlas_driver_offer_bpp.issue_report SET status = 'OPEN' WHERE status = 'NEW';
UPDATE atlas_driver_offer_bpp.issue_report SET status = 'PENDING' WHERE status = 'INPROGRESS';

CREATE INDEX idx_ticket_id ON atlas_driver_offer_bpp.issue_report USING btree (ticket_id);
