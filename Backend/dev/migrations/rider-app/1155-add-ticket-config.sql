INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
VALUES ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'Ticket_Kapture',
    json_build_object
        ('auth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
         'version', 'v.2.0',
         'url', 'nammayatri.kapturecrm.com'
        )
    );
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
VALUES ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'Ticket_Kapture',
    json_build_object
        ('auth', '0.1.0|2|JMM1g9qYbwWtoscHG4qTUql80FhbCz8dMpB0ZMWaUhbSa9RBqO3bUsggftvSo2kCHjJTYgGykwXJzU1xVjDu',
         'version', 'v.2.0',
         'url', 'nammayatri.kapturecrm.com'
        )
    );
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN issue_ticket_service character varying(30) DEFAULT 'Kapture' NOT NULL;

ALTER TABLE atlas_app.issue RENAME COLUMN ride_booking_id to booking_id;
ALTER TABLE atlas_app.issue ADD COLUMN ticket_id character varying(255);
ALTER TABLE atlas_app.issue ADD COLUMN status character varying(255) DEFAULT 'OPEN' NOT NULL;

CREATE INDEX idx_ticket_id ON atlas_app.issue USING btree (ticket_id);
