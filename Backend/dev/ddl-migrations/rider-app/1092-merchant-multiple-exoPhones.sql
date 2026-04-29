-- LOCAL : DO NOT RUN IN MASTER/PROD
ALTER TABLE atlas_app.merchant
    DROP COLUMN IF EXISTS exo_phone,
    DROP COLUMN IF EXISTS exo_phones;

ALTER TABLE atlas_app.merchant
    ADD COLUMN exo_phones character varying(255) [];

ALTER TABLE atlas_app.merchant ALTER COLUMN exo_phones SET NOT NULL;
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN initiate_call SET NOT NULL;