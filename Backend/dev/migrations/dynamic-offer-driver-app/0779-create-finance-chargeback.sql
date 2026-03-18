-- Settlement Dashboard: Create finance_chargeback table for chargeback/dispute management
-- Initiative 2: Settlement Dashboard & Management

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.finance_chargeback (
    id CHARACTER(36) NOT NULL PRIMARY KEY,
    settlement_report_id CHARACTER(36) NOT NULL,
    transaction_id TEXT NOT NULL,
    chargeback_reason_code TEXT NOT NULL,
    chargeback_amount DOUBLE PRECISION NOT NULL,
    chargeback_status TEXT NOT NULL DEFAULT 'OPEN',
    response_deadline TIMESTAMP WITH TIME ZONE NOT NULL,
    evidence_url TEXT,
    admin_notes TEXT,
    merchant_id TEXT NOT NULL,
    merchant_operating_city_id TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

-- Indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_finance_chargeback_settlement_report_id
    ON atlas_driver_offer_bpp.finance_chargeback (settlement_report_id);

CREATE INDEX IF NOT EXISTS idx_finance_chargeback_transaction_id
    ON atlas_driver_offer_bpp.finance_chargeback (transaction_id);

CREATE INDEX IF NOT EXISTS idx_finance_chargeback_status
    ON atlas_driver_offer_bpp.finance_chargeback (chargeback_status);

CREATE INDEX IF NOT EXISTS idx_finance_chargeback_merchant_city
    ON atlas_driver_offer_bpp.finance_chargeback (merchant_id, merchant_operating_city_id);

CREATE INDEX IF NOT EXISTS idx_finance_chargeback_created_at
    ON atlas_driver_offer_bpp.finance_chargeback (created_at);
