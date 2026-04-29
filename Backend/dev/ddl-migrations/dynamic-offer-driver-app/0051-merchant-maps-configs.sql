-- CREATE TABLE atlas_driver_offer_bpp.merchant_service_config (
--     merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
--     service_name character varying(30) NOT NULL,
--     config_json json NOT NULL,
--     updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
--     created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
--     PRIMARY KEY (merchant_id, service_name)
-- );

-- for local testing only
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ALTER COLUMN state DROP NOT NULL;
