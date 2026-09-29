-- HDFC CBX bulk payout: the indexes the status-check job, batch creation and the admin payout APIs
-- read through. No two index the same rows on the same key, and none is a prefix of another.
-- Hand-written rather than generated: three are partial, and the generator sorts index columns,
-- which cannot express (merchant_operating_city_id, created_at).

-- Status-check job (one per city): that city's batches it still owes a call, soonest first
-- (findDueForStatusHit). next_status_call_at is cleared when a batch is
-- finished, so the partial index holds only live batches however many finished ones pile up behind it.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_moc_next_status_call_at
  ON atlas_driver_offer_bpp.payout_batch (merchant_operating_city_id, next_status_call_at)
  WHERE next_status_call_at IS NOT NULL;

-- Batches by execution date: the admin batch list (findAllPayoutBatchesWithFilters: one city over a
-- date range) and seeding the day's file-reference counter (findMaxClientRefNo: one date, every city).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_execution_date_moc
  ON atlas_driver_offer_bpp.payout_batch (execution_date, merchant_operating_city_id);

-- A batch's orders (findAllByBatchId after each status call, findAllByBatchIdWithOptions for the
-- drill-down). payout_order already holds live orders, hence CONCURRENTLY.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_order_batch_id
  ON atlas_driver_offer_bpp.payout_order (batch_id)
  WHERE batch_id IS NOT NULL;

-- Excluded beneficiaries: a city's over a date range, newest first (findExcludedByMocAndTime), and one
-- batch's for the drill-down (findExcludedOfBatch). An exclusion is written while its batch is being
-- claimed, so the batch lookup is a short range on this same index (from the batch's created_at),
-- filtered by batch_id; excluded rows are indexed once.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_excluded_moc_created_at
  ON atlas_driver_offer_bpp.payout_request (merchant_operating_city_id, created_at DESC)
  WHERE status = 'EXCLUDED';
