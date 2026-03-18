-- Indexes to support operator received payout queries
-- These enable efficient aggregation of settled/collected fees by driver and date

-- Composite index for settled fee lookups by driver and date
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_fee_settled_driver_date
ON atlas_driver_offer_bpp.driver_fee (driver_id, created_at)
WHERE status IN ('SETTLED', 'COLLECTED_CASH');

-- Index for pending fee lookups by driver (outstanding payout)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_fee_pending_driver
ON atlas_driver_offer_bpp.driver_fee (driver_id)
WHERE status IN ('PAYMENT_PENDING', 'PAYMENT_OVERDUE');

-- Index on driver_operator_association for active operator lookups (if not already present)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_operator_assoc_operator_active
ON atlas_driver_offer_bpp.driver_operator_association (operator_id)
WHERE is_active = true;
