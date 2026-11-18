-- Bulk payout transaction rows and the driver payout transactions report.

-- A bulk order's row, by the short id HDFC echoes (findByTransactionRef on submit and on each outcome).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_transaction_transaction_ref
  ON atlas_driver_offer_bpp.payout_transaction (transaction_ref);

-- The report's join from a payout order to its transaction row.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_transaction_payout_order_id
  ON atlas_driver_offer_bpp.payout_transaction (payout_order_id);

-- The report's filter: one city over a created_at range.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_moc_created_at
  ON atlas_driver_offer_bpp.payout_request (merchant_operating_city_id, created_at);
