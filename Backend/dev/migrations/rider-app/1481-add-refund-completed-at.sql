-- Backfill existing terminal refunds with updated_at as completion time
UPDATE atlas_app.refunds
SET completed_at = updated_at
WHERE status IN ('REFUND_SUCCESS', 'REFUND_FAILURE')
  AND completed_at IS NULL;
