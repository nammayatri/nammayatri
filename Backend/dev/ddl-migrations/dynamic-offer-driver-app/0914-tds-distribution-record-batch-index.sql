-- TDS disbursement: the send job, the report screen and the batch counts load a batch's records (findAllByBatchId,
-- findAllByBatchIdAndStatusesWithLimit).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_record_batch_id
  ON atlas_driver_offer_bpp.tds_distribution_record (batch_id);
