-- Create operator_invoice table for storing uploaded and platform-generated invoices
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.operator_invoice (
    id                          CHARACTER(36) PRIMARY KEY,
    operator_id                 CHARACTER(36) NOT NULL,
    fleet_owner_id              CHARACTER(36),
    invoice_type                VARCHAR(30) NOT NULL,
    invoice_number              VARCHAR(100),
    invoice_date                TIMESTAMP WITH TIME ZONE NOT NULL,
    amount                      DOUBLE PRECISION NOT NULL,
    currency                    VARCHAR(10) NOT NULL DEFAULT 'INR',
    file_name                   TEXT NOT NULL,
    s3_key                      TEXT NOT NULL,
    remarks                     TEXT,
    status                      VARCHAR(20) NOT NULL DEFAULT 'ACTIVE',
    merchant_id                 CHARACTER(36) NOT NULL,
    merchant_operating_city_id  CHARACTER(36) NOT NULL,
    created_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
    updated_at                  TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

-- Indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_operator_invoice_operator ON atlas_driver_offer_bpp.operator_invoice(operator_id);
CREATE INDEX IF NOT EXISTS idx_operator_invoice_fleet ON atlas_driver_offer_bpp.operator_invoice(fleet_owner_id);
CREATE INDEX IF NOT EXISTS idx_operator_invoice_date ON atlas_driver_offer_bpp.operator_invoice(invoice_date);
CREATE INDEX IF NOT EXISTS idx_operator_invoice_type_status ON atlas_driver_offer_bpp.operator_invoice(invoice_type, status);
