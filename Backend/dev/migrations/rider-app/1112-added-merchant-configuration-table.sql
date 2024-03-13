ALTER TABLE atlas_app.person ADD COLUMN blocked_at TIMESTAMP;

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN use_fraud_detection boolean NOT NULL DEFAULT FALSE;

INSERT INTO atlas_app.merchant_config VALUES (now(), true, 0, '{"period":24, "periodType":"Hours"}', 5, '{"period":24, "periodType":"Hours"}', 0, 0, '{"period":24, "periodType":"Hours"}', 5, '{"period":24, "periodType":"Hours"}', 'id', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'mOpCityId', now());
