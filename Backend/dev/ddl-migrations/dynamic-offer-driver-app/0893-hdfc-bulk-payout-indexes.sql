-- HDFC CBX bulk payout: every index the status-check job, the payout sweep and the admin payout
-- APIs read through. Hand-written rather than generated: the generator sorts index columns, which
-- cannot express (merchant_operating_city_id, created_at).

-- Status-check job: batches it still owes a call, soonest first (findDueForStatusHit,
-- findEarliestNextStatusHitAt). next_status_call_at is cleared when a batch is finished, so the
-- partial index holds only live batches however many finished ones pile up behind it.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_next_status_call_at
  ON atlas_driver_offer_bpp.payout_batch (next_status_call_at)
  WHERE next_status_call_at IS NOT NULL;

-- Admin batch list (findAllPayoutBatchesWithFilters): one city, newest first.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_moc_created_at
  ON atlas_driver_offer_bpp.payout_batch (merchant_operating_city_id, created_at DESC);

-- Seeding the day's file-reference counter (findMaxClientRefNo): the largest reference on one
-- execution date, read as a single index row.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_batch_execution_date_client_ref_no
  ON atlas_driver_offer_bpp.payout_batch (execution_date, client_ref_no);

-- A batch's orders (findAllByBatchId after each status call, findAllByBatchIdWithOptions for the
-- drill-down). payout_order already holds live orders, hence CONCURRENTLY.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_order_batch_id
  ON atlas_driver_offer_bpp.payout_order (batch_id)
  WHERE batch_id IS NOT NULL;

-- Double-pay guard (findByBeneficiaryWithFilters with INITIATED/PROCESSING): run for every candidate
-- by the eligibility pass and again under the claim lock, and by the adhoc and instant paths. Only
-- in-flight rows, so settled history and the excluded rows each sweep adds stay out of it.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_beneficiary_in_flight
  ON atlas_driver_offer_bpp.payout_request (beneficiary_id, created_at DESC)
  WHERE status IN ('INITIATED', 'PROCESSING');

-- Excluded beneficiaries of one batch (findExcludedByBatchId), for the drill-down.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_excluded
  ON atlas_driver_offer_bpp.payout_request (batch_id)
  WHERE status = 'EXCLUDED';

-- Excluded beneficiaries of a city over a date range, newest first (findExcludedByMocAndTime).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payout_request_excluded_moc_created_at
  ON atlas_driver_offer_bpp.payout_request (merchant_operating_city_id, created_at DESC)
  WHERE status = 'EXCLUDED';
