UPDATE atlas_app.merchant_config set id=merchant_id;

ALTER TABLE atlas_app.merchant_config ALTER COLUMN id SET NOT NULL;

ALTER TABLE atlas_app.merchant_config DROP CONSTRAINT merchant_config_pkey;

ALTER TABLE atlas_app.merchant_config ADD PRIMARY KEY (id);
