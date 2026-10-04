-- TDS disbursement: at most one certificate record per person, financial year and quarter. Dashboard confirm reuses
-- the existing record (after a failed send) instead of inserting a second one; this enforces it at the database.
-- Legacy rows (manifest flow) have no financial_year and are left out of the constraint.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_record_driver_fy_quarter
  ON atlas_driver_offer_bpp.tds_distribution_record (driver_id, financial_year, quarter)
  WHERE financial_year IS NOT NULL;
