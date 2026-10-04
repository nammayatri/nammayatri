-- TDS disbursement: the review screen and the validation step load every file of a batch
-- (findAllByBatchId). Without this index each lookup scans the whole tds_distribution_pdf_file table,
-- which grows by up to 500 rows per city per quarter.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_pdf_file_batch_id
  ON atlas_driver_offer_bpp.tds_distribution_pdf_file (batch_id);
