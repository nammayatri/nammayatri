-- HDFC CBX bulk payout indexes. Hand-written: four are partial, and the generator sorts index
-- columns, so it cannot express (merchant_operating_city_id, created_at).

-- Status-check job's due scan per city (findDueForStatusHit). next_status_call_at is cleared when a
-- batch finishes, so the partial index holds only live batches.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_moc_next_status_call_at
  ON atlas_driver_offer_bpp.payout_batch (merchant_operating_city_id, next_status_call_at)
  WHERE next_status_call_at IS NOT NULL;

-- Admin batch list (findAllPayoutBatchesWithFilters): one city over an execution-date range.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_moc_execution_date
  ON atlas_driver_offer_bpp.payout_batch (merchant_operating_city_id, execution_date);

-- Seeds a day's file-reference counter (findMaxClientRefNo); read backwards, it stops at the first row.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_execution_date_client_ref_no
  ON atlas_driver_offer_bpp.payout_batch (execution_date, client_ref_no);

-- A batch's orders (findAllByBatchId, findAllByBatchIdWithOptions).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_order_batch_id
  ON atlas_driver_offer_bpp.payout_order (batch_id)
  WHERE batch_id IS NOT NULL;

-- A batch's excluded requests (findExcludedOfBatch), the Postgres side of the batch_id KV secondary
-- key. Partial, since only bulk requests have a batch.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_batch_id
  ON atlas_driver_offer_bpp.payout_request (batch_id)
  WHERE batch_id IS NOT NULL;

-- Admin worklist of a city's excluded beneficiaries, newest first (findExcludedByMocAndTime).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_excluded_moc_created_at
  ON atlas_driver_offer_bpp.payout_request (merchant_operating_city_id, created_at DESC)
  WHERE status = 'EXCLUDED';