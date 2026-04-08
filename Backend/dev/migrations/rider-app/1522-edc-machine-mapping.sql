CREATE TABLE atlas_app.edc_machine_mapping (
    id VARCHAR(36) PRIMARY KEY,
    person_id VARCHAR(36) NOT NULL,
    merchant_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    terminal_id TEXT NOT NULL,
    merchant_key TEXT NOT NULL,
    merchant_channel_id TEXT NOT NULL,
    paytm_mid TEXT NOT NULL,
    client_id TEXT NOT NULL,
    machine_name TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(36),
    UNIQUE(person_id, merchant_id, merchant_operating_city_id)
);

CREATE INDEX idx_edc_machine_mapping_person ON atlas_app.edc_machine_mapping(person_id);
CREATE INDEX idx_edc_machine_mapping_merchant ON atlas_app.edc_machine_mapping(merchant_id, merchant_operating_city_id);
CREATE INDEX idx_edc_machine_mapping_active ON atlas_app.edc_machine_mapping(person_id, merchant_id, merchant_operating_city_id, is_active);
