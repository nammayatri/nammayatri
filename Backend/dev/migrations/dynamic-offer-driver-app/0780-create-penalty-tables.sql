-- Penalty Engine: Create penalty_rule, penalty_record, and driver_grace_period_tracker tables
-- Initiative: Financial Plan - Penalty & Grace Period Management

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.penalty_rule (
    id CHARACTER(36) NOT NULL PRIMARY KEY,
    merchant_id TEXT NOT NULL,
    merchant_operating_city_id TEXT NOT NULL,
    name TEXT NOT NULL,
    trigger_event TEXT NOT NULL,
    conditions_json TEXT NOT NULL DEFAULT '{}',
    penalty_type TEXT NOT NULL,
    fixed_amount NUMERIC(30,2),
    percentage DOUBLE PRECISION,
    formula_expression TEXT,
    currency TEXT DEFAULT 'INR',
    grace_period_count INT NOT NULL DEFAULT 0,
    grace_period_window_hours INT NOT NULL DEFAULT 24,
    priority INT NOT NULL DEFAULT 0,
    is_active BOOLEAN NOT NULL DEFAULT true,
    start_date TIMESTAMP WITH TIME ZONE,
    end_date TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_penalty_rule_merchant_id
    ON atlas_driver_offer_bpp.penalty_rule (merchant_id);

CREATE INDEX IF NOT EXISTS idx_penalty_rule_merchant_trigger
    ON atlas_driver_offer_bpp.penalty_rule (merchant_id, trigger_event, is_active);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.penalty_record (
    id CHARACTER(36) NOT NULL PRIMARY KEY,
    driver_id CHARACTER(36) NOT NULL,
    rule_id CHARACTER(36) NOT NULL REFERENCES atlas_driver_offer_bpp.penalty_rule(id),
    merchant_id TEXT NOT NULL,
    merchant_operating_city_id TEXT NOT NULL,
    trigger_event CHARACTER VARYING(255) NOT NULL,
    trigger_entity_id TEXT NOT NULL,
    amount NUMERIC(30,2) NOT NULL,
    currency TEXT DEFAULT 'INR',
    reason TEXT NOT NULL,
    status CHARACTER VARYING(255) NOT NULL DEFAULT 'PENDING',
    dispute_reason TEXT,
    dispute_evidence TEXT,
    dispute_resolved_by TEXT,
    dispute_resolved_at TIMESTAMP WITH TIME ZONE,
    ledger_entry_id TEXT,
    invoice_id TEXT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_penalty_record_driver_id
    ON atlas_driver_offer_bpp.penalty_record (driver_id);

CREATE INDEX IF NOT EXISTS idx_penalty_record_rule_id
    ON atlas_driver_offer_bpp.penalty_record (rule_id);

CREATE INDEX IF NOT EXISTS idx_penalty_record_merchant_id
    ON atlas_driver_offer_bpp.penalty_record (merchant_id);

CREATE INDEX IF NOT EXISTS idx_penalty_record_merchant_status
    ON atlas_driver_offer_bpp.penalty_record (merchant_id, status);

CREATE INDEX IF NOT EXISTS idx_penalty_record_driver_status
    ON atlas_driver_offer_bpp.penalty_record (driver_id, status);

CREATE INDEX IF NOT EXISTS idx_penalty_record_created_at
    ON atlas_driver_offer_bpp.penalty_record (created_at);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_grace_period_tracker (
    id CHARACTER(36) NOT NULL PRIMARY KEY,
    driver_id CHARACTER(36) NOT NULL,
    rule_id CHARACTER(36) NOT NULL REFERENCES atlas_driver_offer_bpp.penalty_rule(id),
    merchant_id TEXT NOT NULL,
    merchant_operating_city_id TEXT NOT NULL,
    offense_count INT NOT NULL DEFAULT 0,
    window_start_time TIMESTAMP WITH TIME ZONE NOT NULL,
    window_end_time TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    CONSTRAINT unique_driver_rule UNIQUE (driver_id, rule_id)
);

CREATE INDEX IF NOT EXISTS idx_grace_tracker_driver_id
    ON atlas_driver_offer_bpp.driver_grace_period_tracker (driver_id);

CREATE INDEX IF NOT EXISTS idx_grace_tracker_rule_id
    ON atlas_driver_offer_bpp.driver_grace_period_tracker (rule_id);

CREATE INDEX IF NOT EXISTS idx_grace_tracker_driver_rule
    ON atlas_driver_offer_bpp.driver_grace_period_tracker (driver_id, rule_id);
