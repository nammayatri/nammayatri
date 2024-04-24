-- Please update auth key with prod credentials before deploying
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Ticket_Kapture',
  json_build_object(
    'auth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
    'version', 'v.2.0',
    'url', 'nammayatri.kapturecrm.com'
    )
FROM atlas_app.merchant_operating_city m;


CREATE INDEX idx_ticket_id ON atlas_app.issue USING btree (ticket_id);
