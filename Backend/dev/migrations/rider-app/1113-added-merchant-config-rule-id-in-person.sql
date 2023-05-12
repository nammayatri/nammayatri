ALTER TABLE atlas_app.person ADD COLUMN blocked_by_rule_id character(36);

ALTER TABLE atlas_app.merchant_config ADD COLUMN Simulation_booking_cancellation_count_window json default '{"period":24, "periodType":"Hours"}' NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN Simulation_booking_cancelled_by_driver_count_threshold int default 5 NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN Simulation_booking_cancelled_by_driver_count_window json default '{"period":24, "periodType":"Hours"}' NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN Simulation_search_count_threshold int default 5 NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN Simulation_search_count_window json default '{"period":24, "periodType":"Hours"}' NOT NULL;

ALTER TABLE atlas_app.merchant_config ADD COLUMN id character(36);

UPDATE atlas_app.merchant_config set id=merchant_id;

ALTER TABLE atlas_app.merchant_config ALTER COLUMN id SET NOT NULL;

ALTER TABLE atlas_app.merchant_config DROP CONSTRAINT merchant_config_pkey;

ALTER TABLE atlas_app.merchant_config ADD PRIMARY KEY (id);

ALTER TABLE atlas_app.merchant_config ADD COLUMN enabled boolean default true NOT NULL;

ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_detection_window;
